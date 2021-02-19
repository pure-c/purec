module Main
  ( main
  , compileModule
  ) where


import Prelude

import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.Except (except, runExcept, runExceptT, withExceptT)
import CoreFn.FromJSON (moduleFromJSON) as C
import CoreFn.Module (FilePath(..), Module(..)) as C
import Data.Array as A
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (wrap)
import Data.String as Str
import Data.Traversable (for_, sequence_)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console as Console
import Effect.Exception (Error)
import Effect.Exception as Error
import Foreign (renderForeignError)
import Language.PureScript.CodeGen.C (moduleToAST) as C
import Language.PureScript.CodeGen.C.AST as AST
import Language.PureScript.CodeGen.C.File (dottedModuleName)
import Language.PureScript.CodeGen.C.File as F
import Language.PureScript.CodeGen.C.Pretty (PrintError, prettyPrint) as C
import Language.PureScript.CodeGen.C.Pretty as PrintError
import Language.PureScript.CodeGen.CompileError (CompileError) as C
import Language.PureScript.CodeGen.CompileError as CompileError
import Language.PureScript.CodeGen.SupplyT (runSupplyT)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Path (FilePath)
import Node.Path as FilePath
import Node.Process as Process
import Options.Applicative ((<**>))
import Options.Applicative as OA

type RawOptions =
  { input :: Maybe (Array String)
  , main :: Maybe String
  }

emptyOpts :: RawOptions
emptyOpts =
  { input: Nothing
  , main: Nothing
  }

main :: Effect Unit
main = launchAff_ do
  args <- liftEffect $
    OA.customExecParser
      (OA.prefs
        (OA.multiSuffix "..."))
      (OA.info
        ((ado
          mainModule <-
            OA.strOption
              (OA.long "main" <>
              OA.short 'm' <>
              (OA.help
                ("Specify the name of main module.\n" <>
                "The main module contains the entry point to the final binary.")) <>
              OA.value "Main")
          strictMain <-
            OA.flag true false
              (OA.long "non-strict-main" <>
              (OA.help
                ("If enabled, Main module may return values other than Unit or Int.")))
          inputs <-
            OA.many
              (OA.argument OA.str
                (OA.metavar "<input>" <>
                OA.help "Filepaths of modules to compile"))
        in { mainModule, inputs, strictMain }) <**> OA.helper)
        (OA.fullDesc <>
        OA.header "Pure-C compiler."))

  -- compile modules to C
  for_ args.inputs \modulePath -> do
    Console.log $ "Compiling " <> modulePath <> "..."
    compileModule args.strictMain
      (\(C.Module { moduleName }) ->
        dottedModuleName moduleName == args.mainModule)
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
renderPipelineError (PrintError (PrintError.InvalidStateError msg)) =
  "Compilation failed: invalid state: " <> msg
renderPipelineError (PrintError (PrintError.NotImplementedError msg)) =
  "Compilation failed: not implemented: " <> msg

-- | Compie the given corefn json file to C.
compileModule
  :: Boolean -- ^ strict main checking?
  -> (C.Module _ -> Boolean) -- ^ is main module?
  -> FilePath -- ^ corefn json file
  -> Aff Unit
compileModule strictMain isMain corefn = do
  let
    outputDir =
      FilePath.dirname corefn <> "/.."

  input <- FS.readTextFile UTF8 corefn
  core  <- case runExcept $ C.moduleFromJSON input of
    Right v ->
      pure v
    Left e ->
      throwError $ Error.error $ "Failed to parse Corefn: " <> do
        A.intercalate ", " $ map renderForeignError $ A.fromFoldable e

  let

    { module:
        mod@(C.Module
          { moduleName
          , modulePath: C.FilePath modulePath
          })
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
        ast <- C.moduleToAST strictMain (isMain $ core."module") core."module"
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
