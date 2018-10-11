module Test.Main where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (catchError)
import Data.Array as A
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (wrap)
import Data.String (Pattern(..))
import Data.String as Str
import Data.Traversable (for, for_)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Language.PureScript.CodeGen.C.Pretty (PrintError) as C
import Language.PureScript.CodeGen.CompileError (CompileError) as C
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Path (FilePath)
import Test.Spec (describe, it)
import Test.Spec.Reporter.Console (consoleReporter) as Spec
import Test.Spec.Runner (defaultConfig, run') as Spec
import Test.Utils (runProc)

data PipelineError
  = CompileError C.CompileError
  | PrintError C.PrintError

main :: Effect Unit
main =
  let
    testsDirectory =
      "upstream/tests/purs/passing"
    outputDir =
      ".tmp/output"
  in launchAff_ do
    FS.writeTextFile UTF8 (outputDir <> "/Makefile") makefileContents
    tests <-
      A.take 1 <$>
      A.dropWhile (\test -> test.name /= "2609") <$>
        discoverPureScriptTests testsDirectory
    runProc "make" [ "clean", "-C", outputDir ]
    -- TODO: build the dependencies once before starting the actual test suites.
    --       otherwise the first test will likely time out and give skewed
    --       results in terms of timing. probably the easiest is to write a
    --       dummy 'Main' module somewhere, write it to the "<outputDir>/sources"
    --       file and compile it like any other module.
    liftEffect $
      Spec.run' (Spec.defaultConfig { timeout = Just 30000 }) [Spec.consoleReporter] $
        describe "PureScript's 'passing' tests" $
          for_ tests \test ->
            it test.name do
              mkdirp outputDir
              FS.writeTextFile UTF8 (outputDir <> "/sources") $
                A.intercalate "\n" test.files
              runProc "make" [ "-s", "-j", "16", "-C", outputDir ]
              runProc (outputDir <> "/main.out") []

makefileContents :: String
makefileContents = """
default: premain

PUREC_DIR := ../..
include $(PUREC_DIR)/mk/target.mk

SHELL := /bin/bash

srcs := $(addprefix ../../,$(shell cat sources))
deps := $(shell\
	find "$(PUREC_DIR)"/upstream/tests/support/bower_components/purescript-{control,assert,effect,prelude,console,functions,identity,either,integers,bifunctors,orders,newtype,type-equality,math,distributive,refs,unsafe-coerce,st,lazy,foldable-traversable,unfoldable,partial,tuples,maybe,newtype,invariant,tailrec}/src/\
	    -type f\
	    -name '*.purs')

premain: $(srcs)
	@touch $^
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
