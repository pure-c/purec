module Main
  ( main
  , compileModule
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.Except (except, runExcept, runExceptT, withExceptT)
import CoreFn.FromJSON as C
import CoreFn.Module as C
import Data.Array as A
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (wrap)
import Data.String as Str
import Data.Traversable (for_, sequence, sequence_)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console as Console
import Effect.Exception (Error)
import Effect.Exception as Error
import Language.PureScript.CodeGen.C as C
import Language.PureScript.CodeGen.C.AST as AST
import Language.PureScript.CodeGen.C.File (dottedModuleName)
import Language.PureScript.CodeGen.C.File as F
import Language.PureScript.CodeGen.C.Pretty as C
import Language.PureScript.CodeGen.C.Pretty as PrintError
import Language.PureScript.CodeGen.CompileError as C
import Language.PureScript.CodeGen.CompileError as CompileError
import Language.PureScript.CodeGen.SupplyT (runSupplyT)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Path (FilePath)
import Node.Path as FilePath
import Node.Process as Process
import POSIX.GetOpt.Monad (GetOptT, getopt, runGetOptT)
import POSIX.GetOpt.Monad as GetOpt

type RawOptions =
  { input :: Maybe (Array String)
  , main :: Maybe String
  }

emptyOpts :: RawOptions
emptyOpts =
  { input: Nothing
  , main: Nothing
  }

help = """Usage: purec [-h] [--main <name>] <input>...

Options:
  -h, --help
    Show this help and exit.
  -m, --main <name>
    Specify the name of main module.
    The main module contains the entry point to the final binary.
"""

main :: Effect Unit
main = launchAff_ do
  -- parse command line
  rawOpts <-
    let
      go :: RawOptions -> GetOptT Effect RawOptions
      go opts =
        getopt >>= case _ of
          Just { option: 'h' } ->
            liftEffect do
              Console.log help
              Process.exit 0
          Just { option: 'm', optarg: Just arg } ->
            go $ opts { main = Just arg }
          Just _ ->
            go opts
          Nothing -> do
            optind <- GetOpt.optind
            argv   <- GetOpt.argv
            pure $
              opts
                { input = Just $ A.drop optind argv
                }
    in liftEffect do
      argv <- Process.argv
      runGetOptT "m:(main)h(help)" argv 2 $
        go emptyOpts

  -- validate options
  mainModuleName <- pure $ rawOpts.main <|> Just "Main"
  modulePaths    <- pure $ fromMaybe [] $ rawOpts.input

  -- compile modules to C
  for_ modulePaths \modulePath -> do
    Console.log $ "Compiling " <> modulePath <> "..."
    compileModule
      (\(C.Module { moduleName }) ->
        maybe false ((dottedModuleName moduleName) == _) mainModuleName)
      modulePath

  where
  die :: ∀ m a. MonadEffect m => String -> m a
  die msg =
    liftEffect do
      Console.error msg
      Process.exit 1

data PipelineError
  = CompileError C.CompileError
  | PrintError C.PrintError

-- | XXX: improve this
renderPipelineError :: PipelineError -> String
renderPipelineError (CompileError (CompileError.InternalError msg)) =
  "Compilation failed: internal error: " <> msg
renderPipelineError (CompileError (CompileError.NotImplementedError msg)) =
  "Compilation failed: not implemented: " <> msg
renderPipelineError (PrintError (PrintError.InternalError msg)) =
  "Compilation failed: internal error: " <> msg
renderPipelineError (PrintError (PrintError.NotImplementedError msg)) =
  "Compilation failed: not implemented: " <> msg

-- | Compie the given corefn json file to C.
compileModule
  :: (C.Module _ -> Boolean) -- ^ is main module?
  -> FilePath -- ^ corefn json file
  -> Aff Unit
compileModule isMain corefn = do
  let
    outputDir =
      FilePath.dirname corefn <> "/.."

  input <- FS.readTextFile UTF8 corefn
  core  <- case runExcept $ C.moduleFromJSON input of
    Right v ->
      pure v
    Left _ ->
      throwError $ Error.error "Failed to parse Corefn"

  let

    { module:
        mod@C.Module
          { moduleName
          , modulePath: C.FilePath modulePath
          }
    } =
      core

    sourceFilePaths =
      let
        mkSourcePath ext = (_ <> ext) <<<
          ((FilePath.dirname modulePath <> "/") <> _) <$>
            Str.stripSuffix (wrap ".purs") (FilePath.basename modulePath)
      in
        { ffi:
            { c: mkSourcePath ".c"
            , h: mkSourcePath ".h"
            }
        }

    targetFilePaths =
      let
        mkOutputFilePath ext =
          outputDir <> "/" <> F.cModulePath moduleName <> ext
      in
        { h: mkOutputFilePath ".h"
        , c: mkOutputFilePath ".c"
        , ffi:
            { h: mkOutputFilePath "_ffi.h"
            , c: mkOutputFilePath "_ffi.c"
            }
        }

  -- Perform in-memory compilation to C
  { header, implementation } <-
    -- TODO: add `Show` instance for errors
    either (throwError <<< Error.error <<< renderPipelineError) pure =<< do
     runSupplyT $ runExceptT do

      -- derive the C AST
      { header, implementation } <- withExceptT CompileError ado
        ast <- C.moduleToAST (isMain $ core."module") core."module"
        in
          let
            { init: header
            , rest: implementation
            } = A.span (notEq AST.EndOfHeader) ast
          in
            { header, implementation }

      -- pretty print the AST
      withExceptT PrintError ado
        header'         <- except $ C.prettyPrint header
        implementation' <- except $ C.prettyPrint implementation
        in { header: header', implementation: implementation' }

  -- Emit to disk
  do
    let
      cpTextFile src dst =
        FS.writeTextFile UTF8 dst =<<
          FS.readTextFile UTF8 src
    sequence_
      [ do
          mkdirp $ FilePath.dirname targetFilePaths.h
          FS.writeTextFile UTF8 targetFilePaths.h header
      , do
          mkdirp $ FilePath.dirname targetFilePaths.c
          FS.writeTextFile UTF8 targetFilePaths.c implementation
      , for_ sourceFilePaths.ffi.h \sourceFile ->
          whenM (FS.exists sourceFile) do
            mkdirp $ FilePath.dirname targetFilePaths.ffi.h
            cpTextFile sourceFile targetFilePaths.ffi.h
      , for_ sourceFilePaths.ffi.c \sourceFile ->
          FS.exists sourceFile >>=
            if _
              then
                Just targetFilePaths.ffi.c <$ do
                  mkdirp $ FilePath.dirname targetFilePaths.ffi.c
                  cpTextFile sourceFile targetFilePaths.ffi.c
              else pure Nothing
      ]

-- TODO Pick up a library for this
-- XXX should work recursively
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

foreign import errorCodeImpl
  :: ∀ a
   . Maybe a
  -> (a -> Maybe a)
  -> Error
  -> Maybe String

errorCode :: Error -> Maybe String
errorCode = errorCodeImpl Nothing Just
