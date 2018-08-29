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
import Debug.Trace (spy, traceM)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.Class (liftAff)
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
import Node.Path as FilePath
import Partial.Unsafe (unsafePartial)
import Test.Compile (runClang, runProc)

data PipelineError
  = CompileError C.CompileError
  | PrintError C.PrintError

main :: Effect Unit
main = launchAff_ do
  exampleFiles <- FS.readdir "examples"
  suites <-
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
            , directory: "examples"
            , files:
                ("/home/felix/projects/purescript-c-prelude/src/Type/Data/RowList.purs") A.:
                ("/home/felix/projects/purescript-c-prelude/src/Type/Data/Row.purs") A.:
                ("/home/felix/projects/purescript-c-prelude/src/Data/BooleanAlgebra.purs") A.:
                ("/home/felix/projects/purescript-c-prelude/src/Data/Ordering.purs") A.:
                ("/home/felix/projects/purescript-c-prelude/src/Data/Ord/Unsafe.purs") A.:
                ("/home/felix/projects/purescript-c-prelude/src/Data/Ord.purs") A.:
                ("/home/felix/projects/purescript-c-prelude/src/Data/CommutativeRing.purs") A.:
                ("/home/felix/projects/purescript-c-prelude/src/Data/EuclideanRing.purs") A.:
                ("/home/felix/projects/purescript-c-prelude/src/Data/Function.purs") A.:
                ("/home/felix/projects/purescript-c-prelude/src/Data/Functor.purs") A.:
                ("/home/felix/projects/purescript-c-prelude/src/Data/Symbol.purs") A.:
                ("/home/felix/projects/purescript-c-prelude/src/Data/Show.purs") A.:
                ("/home/felix/projects/purescript-c-prelude/src/Data/Field.purs") A.:
                ("/home/felix/projects/purescript-c-prelude/src/Data/DivisionRing.purs") A.:
                ("/home/felix/projects/purescript-c-prelude/src/Data/Unit.purs") A.:
                ("/home/felix/projects/purescript-c-prelude/src/Data/Monoid.purs") A.:
                ("/home/felix/projects/purescript-c-prelude/src/Data/Bounded.purs") A.:
                ("/home/felix/projects/purescript-c-prelude/src/Prelude.purs") A.:
                ("/home/felix/projects/purescript-c-prelude/src/Data/Monoid/Disj.purs") A.:
                ("/home/felix/projects/purescript-c-prelude/src/Data/Monoid/Conj.purs") A.:
                ("/home/felix/projects/purescript-c-prelude/src/Data/Monoid/Additive.purs") A.:
                ("/home/felix/projects/purescript-c-prelude/src/Data/Monoid/Dual.purs") A.:
                ("/home/felix/projects/purescript-c-prelude/src/Data/Monoid/Endo.purs") A.:
                ("/home/felix/projects/purescript-c-prelude/src/Data/Monoid/Multiplicative.purs") A.:
                ("/home/felix/projects/purescript-c-prelude/src/Data/NaturalTransformation.purs") A.:
                ("/home/felix/projects/purescript-c-prelude/src/Data/Void.purs") A.:
                ("/home/felix/projects/purescript-c-prelude/src/Data/Eq.purs") A.:
                ("/home/felix/projects/purescript-c-prelude/src/Data/Boolean.purs") A.:
                ("/home/felix/projects/purescript-c-prelude/src/Data/HeytingAlgebra.purs") A.:
                ("/home/felix/projects/purescript-c-prelude/src/Data/Semiring.purs") A.:
                ("/home/felix/projects/purescript-c-prelude/src/Data/Ring.purs") A.:
                ("/home/felix/projects/purescript-c-prelude/src/Data/Semigroup.purs") A.:
                ("/home/felix/projects/purescript-c-prelude/src/Data/Semigroup/Last.purs") A.:
                ("/home/felix/projects/purescript-c-prelude/src/Data/Semigroup/First.purs") A.:
                ("/home/felix/projects/purescript-c-prelude/src/Control/Apply.purs") A.:
                ("/home/felix/projects/purescript-c-prelude/src/Control/Applicative.purs") A.:
                ("/home/felix/projects/purescript-c-prelude/src/Control/Bind.purs") A.:
                ("/home/felix/projects/purescript-c-prelude/src/Control/Monad.purs") A.:
                ("/home/felix/projects/purescript-c-prelude/src/Control/Semigroupoid.purs") A.:
                ("/home/felix/projects/purescript-c-prelude/src/Control/Category.purs") A.:
                ("/home/felix/projects/purescript-c-prelude/src/Record/Unsafe.purs") A.:
                ("examples/" <> file) A.:
                ((("examples/" <> moduleName) <> _) <$> subModules)
            }

  for_ suites \test ->
    let
      pursOutputDir =
        ".tmp/output/" <> test.name
      cOutputDir =
        ".tmp/sources/" <> test.name
    in do
      -- compile each module to it's corefn json rep
      runProc "purs" $
        [ "compile"
        , "-o", pursOutputDir
        , "-g", "corefn"
        ] <> test.files

      Console.log "Compiling all purescript to C..."
      srcs <- do
        -- compile each module's corefn json rep to c
        FS.readdir pursOutputDir >>= traverse \moduleName ->
          let
            corefnJsonFile =
              pursOutputDir <> "/" <> moduleName <> "/corefn.json"
          in do
            Console.log $ "Compiling to C: " <> moduleName <> "..."
            emitC (moduleName == test.name) cOutputDir corefnJsonFile

      Console.log "Compiling C sources..."
      runClang $ [ "-I", cOutputDir, "-o", "a.out" ] <> A.concat srcs

      Console.log "Running module..."
      runProc "./a.out" []

  where
  emitC isMain outputDir jsonFile = do
    input <- FS.readTextFile UTF8 jsonFile
    core  <- case runExcept $ C.moduleFromJSON input of
      Right v ->
        pure v
      Left _ ->
        throwError $ error "Failed to parse Corefn"

    let
      { module: mod@C.Module { moduleName, modulePath: C.FilePath modulePath } } = core
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

    { header, implementation } <-
      -- TODO: add `Show` instance for errors
      either (throwError <<< error <<< const "FAILURE" <<< spy "compile") pure =<< do
        runSupplyT $ runExceptT do
          ast <-
            withExceptT CompileError $
              C.moduleToAST isMain core."module"
          let
            { init: headerAst, rest: implAst } =
              A.span (notEq AST.EndOfHeader) ast
          header         <- withExceptT PrintError $ except $ C.prettyPrint headerAst
          implementation <- withExceptT PrintError $ except $ C.prettyPrint implAst
          pure { header, implementation }

    map A.catMaybes $ liftAff $
      -- XXX this should be pulled from a library somewhere
      let
        cpTextFile src dst =
          FS.writeTextFile UTF8 dst =<<
            FS.readTextFile UTF8 src
      in
        sequence
          [ Nothing <$ do
              mkdirp $ FilePath.dirname targetFilePaths.h
              FS.writeTextFile UTF8 targetFilePaths.h header
          , Just targetFilePaths.c <$ do
              mkdirp $ FilePath.dirname targetFilePaths.c
              FS.writeTextFile UTF8 targetFilePaths.c implementation
          , case sourceFilePaths.ffi.h of
              Nothing ->
                pure Nothing
              Just sourceFile ->
                Nothing <$ do
                  whenM (FS.exists sourceFile) do
                    mkdirp $ FilePath.dirname targetFilePaths.ffi.h
                    cpTextFile sourceFile targetFilePaths.ffi.h
          , case sourceFilePaths.ffi.c of
              Nothing ->
                pure Nothing
              Just sourceFile ->
                FS.exists sourceFile >>=
                  if _
                    then
                      Just targetFilePaths.ffi.c <$ do
                        mkdirp $ FilePath.dirname targetFilePaths.ffi.c
                        cpTextFile sourceFile targetFilePaths.ffi.c
                    else pure Nothing
          ]

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
