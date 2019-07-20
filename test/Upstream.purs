module Test.Upstream
  ( buildUpstreamTestSuite
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (catchError)
import Control.Parallel (parallel, sequential)
import Data.Array as A
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as Str
import Data.Traversable (for, for_)
import Effect.Aff (Aff)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Path (FilePath)
import Test.Spec (Spec, describe, it, pending)
import Test.Utils (errorCode, mkdirp, runProc)

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

buildUpstreamTestSuite :: Aff (Spec Unit)
buildUpstreamTestSuite =
  let
    testsDirectory =
      "upstream/tests/purs/passing"
    outputDir =
      ".tmp/output"
    cacheDir =
      outputDir <> ".cache"
  in ado
    tests <- sequential ado
      parallel $ prepareCacheDir cacheDir
      tests <- parallel $ discoverPureScriptTests testsDirectory
      in tests
    in
      describe "PureScript's 'passing' tests" $
        for_ tests case _ of
          { name } | name `A.elem` blackList  ->
            pending name
          { name, files } ->
            it name do
              runProc "rm" [ "-rf", outputDir ]
              runProc "rsync" [ "-a", cacheDir <> "/", outputDir <> "/" ]
              make outputDir files >>= runProc <@> []

-- | Run make, return the produced output
make :: FilePath -> Array FilePath -> Aff FilePath
make dir pursSources =
  dir <> "/main.out" <$ do
    FS.writeTextFile UTF8 (dir <> "/sources") $
      A.intercalate "\n" pursSources
    runProc "make" [ "-s", "-j", "16", "-C", dir ]

-- | prepare the output directory and build the project at least once
prepareCacheDir :: FilePath -> Aff Unit
prepareCacheDir dir = do
  mkdirp dir
  FS.writeTextFile UTF8 (dir <> "/Makefile") """
default: premain
.PHONY: default

PUREC_DIR := ../..
include $(PUREC_DIR)/mk/target.mk
SPAGO := PATH=$$PATH:$(PUREC_DIR)/node_modules/.bin spago
PURS := PATH=$$PATH:$(PUREC_DIR)/node_modules/.bin purs
SHELL := /bin/bash
srcs := $(addprefix ../../,$(shell cat sources))

premain: $(srcs)
	@touch $^ || { :; }
	@cp "$(PUREC_DIR)"/package-sets/* .
	@cp "$(PUREC_DIR)"/upstream/tests/support/spago.dhall .
	@$(SPAGO) install
	@$(MAKE) -s main

$(eval $(call purs_mk_target,main,Main,$(srcs)))
"""
  FS.writeTextFile UTF8 (dir <> "/Main.purs") """
module Main where
data Unit = Unit
main :: Unit -> Unit
main _ = Unit
"""
  void $ make dir [dir <> "/Main.purs"]
  void $ runProc "rm" [ "-f", dir <> "/Main.purs"]

discoverPureScriptTests
  :: FilePath
  -> Aff
      (Array
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
