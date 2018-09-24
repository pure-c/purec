module Test.Main where

import Prelude

import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.Except (ExceptT(..), except, runExcept, runExceptT, withExcept, withExceptT)
import Control.Monad.Except.Trans (catchError)
import Control.Monad.Trans.Class (lift)
import Control.Parallel (parSequence, parSequence_)
import CoreFn.FromJSON as C
import CoreFn.Module as C
import CoreFn.Names as C
import Data.Array as A
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either, fromRight)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (wrap)
import Data.String (Pattern(..))
import Data.String as Str
import Data.Traversable (for, for_, sequence, traverse, traverse_)
import Debug.Trace (spy, trace, traceM)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Console (log)
import Effect.Exception (Error, error)
import Language.PureScript.CodeGen.C as C
import Language.PureScript.CodeGen.C.AST as AST
import Language.PureScript.CodeGen.C.File as F
import Language.PureScript.CodeGen.C.Pretty as C
import Language.PureScript.CodeGen.Common as C
import Language.PureScript.CodeGen.CompileError as C
import Language.PureScript.CodeGen.SupplyT (runSupplyT)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Path (FilePath)
import Node.Path as FilePath
import Partial.Unsafe (unsafePartial)
import Test.Spec (describe, it)
import Test.Spec.Reporter.Console as Spec
import Test.Spec.Runner as Spec
import Test.Utils (runProc)
import Main as Main

data PipelineError
  = CompileError C.CompileError
  | PrintError C.PrintError

main :: Effect Unit
main =
  let
    testsDirectory =
      "upstream/tests/purs/passing"
  in launchAff_ do
    tests <-
      A.take 1 <$>
      -- A.dropWhile (\test -> test.name /= "2018") <$>
        discoverPureScriptTests testsDirectory
    liftEffect $
      Spec.run' (Spec.defaultConfig { timeout = Just 20000 }) [Spec.consoleReporter] do
        describe "PureScript's 'passing' tests" do
          for_ tests \test ->
            it test.name $
              let
                outputDir =
                  ".tmp/output/" <> test.name
              in do
                mkdirp outputDir
                FS.writeTextFile UTF8 (outputDir <> "/sources") $
                  A.intercalate "\n" test.files
                FS.writeTextFile UTF8 (outputDir <> "/Makefile") $
                  makefileContents
                runProc "make" [ "-s", "-j", "16", "-C", outputDir ]
                runProc (outputDir <> "/main.out") []

makefileContents :: String
makefileContents = """
default: main
.PHONY: default

PUREC_DIR := ../../..
export PATH := $(PUREC_DIR)/node_modules/.bin:$(PATH)
include $(PUREC_DIR)/mk/target.mk

SHELL := /bin/bash

srcs := $(addprefix ../../../,$(shell cat sources))
deps := $(shell\
	find "$(PUREC_DIR)"/upstream/tests/support/bower_components/purescript-{control,effect,prelude,console}/src/\
	    -type f\
	    -name '*.purs')

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
