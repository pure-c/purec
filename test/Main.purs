module Test.Main where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (catchError)
import Control.Parallel (parSequence_, parTraverse_, parallel, sequential)
import Data.Array as A
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (wrap)
import Data.String (Pattern(..))
import Data.String as Str
import Data.Traversable (for, for_)
import Debug.Trace (traceM)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Language.PureScript.CodeGen.C.Pretty (PrintError) as C
import Language.PureScript.CodeGen.CompileError (CompileError) as C
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Path (FilePath)
import Test.Spec (describe, it, pending)
import Test.Spec.Reporter as Spec
import Test.Spec.Runner (defaultConfig, run') as Spec
import Test.Utils (runProc)

data PipelineError
  = CompileError C.CompileError
  | PrintError C.PrintError

-- | A list of (currently) unsupported tests from upstream
blackList :: Array String
blackList =
  [ -- compat issues:
    "NumberLiterals" -- due to show instance for numbers

    -- corefn issues:
  , "NegativeIntInRange"
  , "StringEdgeCases"    -- https://github.com/paulyoung/purescript-corefn/issues/57
  , "StringEscapes"      -- https://github.com/paulyoung/purescript-corefn/issues/57

    -- missing dependencies:
  , "GenericsRep"
      -- depends on:
      --   + purescript-enums (https://github.com/pure-c/pure-c/issues/35)
  ]

data Test = Pending

main :: Effect Unit
main =
  let
    testsDirectory =
      "upstream/tests/purs/passing"
    outputDir =
      ".tmp/output"
    outputDirCache =
      outputDir <> ".cache"
  in launchAff_ do

    tests <- sequential ado
      parallel do
        mkdirp outputDirCache
        FS.writeTextFile UTF8 (outputDirCache <> "/Makefile") makefileContents
        FS.writeTextFile UTF8 (outputDirCache <> "/Main.purs") """
module Main where
data Unit = Unit
main :: Unit -> Unit
main _ = Unit
"""
        void $ make outputDirCache [outputDirCache <> "/Main.purs"]
        void $ runProc "rm" [ "-f", outputDirCache <> "/Main.purs"]
      tests <- parallel $ discoverPureScriptTests testsDirectory
      in tests

    liftEffect $
      Spec.run' (Spec.defaultConfig { timeout = Just 10000 }) [Spec.specReporter] $
        describe "PureScript's 'passing' tests" $
          for_ tests case _ of
            { name } | name `A.elem` blackList  ->
              pending name
            { name, files } ->
              it name do
                runProc "rm" [ "-rf", outputDir ]
                runProc "rsync" [ "-a", outputDirCache <> "/", outputDir <> "/" ]
                make outputDir files >>= runProc <@> []

    where
    make outputDir sources =
      outputDir <> "/main.out" <$ do
        FS.writeTextFile UTF8 (outputDir <> "/sources") $
          A.intercalate "\n" sources
        runProc "make" [ "-s", "-j", "16", "-C", outputDir ]

makefileContents :: String
makefileContents = """
default: premain

PUREC_DIR := ../..
export PATH := $(PUREC_DIR)/node_modules/.bin:$(PATH)
include $(PUREC_DIR)/mk/target.mk

SHELL := /bin/bash

srcs := $(addprefix ../../,$(shell cat sources))
deps := $(shell\
	find "$(PUREC_DIR)"/upstream/tests/support/bower_components/purescript-{proxy,typelevel-prelude,arrays,control,assert,effect,prelude,console,functions,identity,either,integers,bifunctors,orders,newtype,type-equality,math,distributive,refs,unsafe-coerce,st,lazy,foldable-traversable,unfoldable,partial,tuples,maybe,newtype,invariant,tailrec,nonempty}/src/\
	    -type f\
	    -name '*.purs')

premain: $(srcs)
	@touch $^ || { :; }
	@$(MAKE) -s main

.PHONY: default

$(eval $(call purs_mk_target,main,Main,$(srcs),$(deps)))
"""

discoverPureScriptTests
  :: FilePath
  -> Aff (Array
            { name :: String
            , directory :: FilePath
            , files :: Array FilePath
            })
discoverPureScriptTests testsDirectory = do
  testFiles <- FS.readdir testsDirectory
  map A.catMaybes $
    for testFiles \file ->
      for (Str.stripSuffix (Pattern ".purs") file) \moduleName -> ado
        subModules <-
          (FS.readdir $ testsDirectory <> "/" <> moduleName)
            `catchError` \e ->
              if errorCode e == Just "ENOENT"
                  then pure []
                  else throwError e
        in
          { name: moduleName
          , directory: testsDirectory
          , files:
              (testsDirectory <> "/" <> file) A.:
                (((testsDirectory <> "/" <> moduleName <> "/") <> _) <$> subModules)
          }

foreign import errorCodeImpl
  :: ∀ a
   . Maybe a
  -> (a -> Maybe a)
  -> Error
  -> Maybe String

errorCode :: Error -> Maybe String
errorCode = errorCodeImpl Nothing Just

-- TODO Pick up a library for this
mkdirp :: String -> Aff Unit
mkdirp dir = go Nothing (Str.split (wrap "/") dir)
  where
  go cd xs
    | Just { head: x, tail: xs' } <- A.uncons xs
    =
      let
        cd' =
          maybe "" (_ <> "/") cd <> x
      in do
        unless (Str.null cd') do
          mkdir cd'
          go (Just cd') xs'
  go _ _ =
    pure unit

  mkdir dir' =
    FS.mkdir dir' `catchError` \e ->
      unless (errorCode e == Just "EEXIST") do
        throwError e
