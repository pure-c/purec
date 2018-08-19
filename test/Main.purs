module Test.Main where

import Prelude

import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.Except (ExceptT(..), except, runExcept, runExceptT, withExcept, withExceptT)
import Control.Monad.Except.Trans (catchError)
import Control.Monad.Trans.Class (lift)
import Control.Parallel (parSequence_)
import CoreFn.FromJSON as C
import CoreFn.Module as C
import Data.Array as A
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either, fromRight)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as Str
import Data.Traversable (for, for_, traverse_)
import Debug.Trace (spy, traceM)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console as Console
import Effect.Console (log)
import Effect.Exception (Error, error)
import Language.PureScript.CodeGen.C as C
import Language.PureScript.CodeGen.C.AST as AST
import Language.PureScript.CodeGen.C.Pretty as C
import Language.PureScript.CodeGen.Common as C
import Language.PureScript.CodeGen.CompileError as C
import Language.PureScript.CodeGen.SupplyT (runSupplyT)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Partial.Unsafe (unsafePartial)
import Test.Compile (runClang, runProc)

data PipelineError
  = CompileError C.CompileError
  | PrintError C.PrintError

main :: Effect Unit
main = launchAff_ do
  exampleFiles <- FS.readdir "examples"
  modules <-
    map A.catMaybes $
      for exampleFiles \file ->
        for (Str.stripSuffix (Pattern ".purs") file) \moduleName -> ado
          subModules <-
            (FS.readdir $ "examples/" <> moduleName)
              `catchError` \e ->
                if errorCode e == Just "ENOENT"
                   then pure []
                   else throwError e
          in
            { name: moduleName
            , files:
                ("examples/" <> file)
                  A.:
                    ((("examples/" <> moduleName) <> _) <$> subModules)
            }

  for_ modules \{ name, files } ->
    let
      outputDir =
        ".output/" <> name
    in do
      -- compile each module to it's corefn json rep
      runProc "purs" $
        [ "compile"
        , "-o", outputDir
        , "-g", "corefn"
        ] <> files

      -- compile each module's corefn json rep to cpp
      FS.readdir outputDir >>= \fileNames -> do

        Console.log "Compiling to C"
        srcs <- A.concat <$>
          for fileNames \moduleName ->
            let
              moduleOutputDir =
                outputDir <> "/" <> moduleName
              corefnJsonFile =
                moduleOutputDir <> "/corefn.json"
            in do
              Console.log $ "Compiling to C: " <> moduleName <> "..."
              compileToC moduleOutputDir corefnJsonFile

        Console.log "Compiling C sources..."
        runClang $ srcs <> [ "-I", outputDir, "-o", "a.out" ]
        runProc "./a.out" []

      -- TODO: runMain (if any)

  where
  compileToC outputDir jsonFile = do
    input <- FS.readTextFile UTF8 jsonFile
    core  <- case runExcept $ C.moduleFromJSON input of
      Right v ->
        pure v
      Left _ ->
        throwError $ error "Failed to parse Corefn"

    let
      { module: mod@C.Module { moduleName } } =
        core
      implFileName =
        C.runModuleName moduleName <> ".c"
      headerFileName =
        C.runModuleName moduleName <> ".h"
      implFilePath =
        outputDir <> "/" <> implFileName
      headerFilePath =
        outputDir <> "/" <> headerFileName

    -- compile the module to cpp and write header and implementation files
    -- TODO: add `Show` instance for errors
    [ implFilePath ] <$ do
     either (throwError <<< error <<< const "FAILURE" <<< spy "compile") pure =<< do
      runSupplyT $ runExceptT do
        ast <-
          withExceptT CompileError $
            C.moduleToAST core."module"

        let
          { init: headerAst, rest: implAst } =
            A.span (notEq AST.EndOfHeader) ast

        headerSrc <- withExceptT PrintError $ except $ C.prettyPrint headerAst
        implSrc   <- withExceptT PrintError $ except $ C.prettyPrint implAst

        liftAff do
          FS.mkdir outputDir `catchError` \e ->
            unless (errorCode e == Just "EEXIST") do
              throwError e
          parSequence_
            [ FS.writeTextFile UTF8 headerFilePath headerSrc
            , FS.writeTextFile UTF8 implFilePath implSrc
            ]

foreign import errorCodeImpl
  :: ∀ a
   . Maybe a
  -> (a -> Maybe a)
  -> Error
  -> Maybe String

errorCode :: Error -> Maybe String
errorCode = errorCodeImpl Nothing Just
