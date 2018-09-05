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

data PipelineError
  = CompileError C.CompileError
  | PrintError C.PrintError

main :: Effect Unit
main =
  let
    testsDirectory =
      "tests/purs/passing"
  in launchAff_ do
    tests <-
      -- A.take 1 <<<
      -- A.dropWhile (\test -> test.name /= "2172") <$>
      --   discoverPureScriptTests testsDirectory
    liftEffect $
      Spec.run' (Spec.defaultConfig { timeout = Just 20000 }) [Spec.consoleReporter] do
        describe "PureScript's 'passing' tests" do
          for_ tests \test ->
            it test.name $
              let
                pursOutputDir =
                  ".tmp/output/" <> test.name
                cOutputDir =
                  ".tmp/sources/" <> test.name
              in do
                -- compile each module to it's corefn json rep
                Console.log "Compiling PureScript to CoreFn..."
                runProc "purs" $
                  [ "compile"
                  , "-o", pursOutputDir
                  , "-g", "corefn"
                  ] <> test.files

                -- compile each module's corefn json rep to c
                Console.log "Compiling CoreFn to C..."
                void $
                  FS.readdir pursOutputDir >>= traverse \moduleName ->
                    let
                      corefnJsonFile =
                        pursOutputDir <> "/" <> moduleName <> "/corefn.json"
                    in do
                      Console.log $ "Compiling to C: " <> moduleName <> "..."
                      compile
                        (moduleName == test.name || moduleName == "Main")
                        cOutputDir
                        corefnJsonFile

                -- generate Makefile
                FS.writeTextFile UTF8 (cOutputDir <> "/Makefile") tempMakeFile

                -- build the project using Makefile (will produce an executable
                -- called 'a.out'
                runProc "make"
                  [ "-j", "4"
                  , "-C", cOutputDir
                  ]

                -- run the built executable
                runProc (cOutputDir <> "/a.out") []

compile
  :: Boolean  -- ^ does this module contain  the 'main' entrypoint?
  -> FilePath -- ^ output directory
  -> FilePath -- ^ corefn json file
  -> Aff (Array FilePath)
compile isMain outputDir jsonFile = do
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
              -- * purescript-prelude
              let
                purescriptPrelude =
                  map
                    ("bower_components/purescript-prelude" <> _)
                    [ "/src/Type/Data/RowList.purs"
                    , "/src/Type/Data/Row.purs"
                    , "/src/Data/BooleanAlgebra.purs"
                    , "/src/Data/Ordering.purs"
                    , "/src/Data/Ord/Unsafe.purs"
                    , "/src/Data/Ord.purs"
                    , "/src/Data/CommutativeRing.purs"
                    , "/src/Data/EuclideanRing.purs"
                    , "/src/Data/Function.purs"
                    , "/src/Data/Functor.purs"
                    , "/src/Data/Symbol.purs"
                    , "/src/Data/Show.purs"
                    , "/src/Data/Field.purs"
                    , "/src/Data/DivisionRing.purs"
                    , "/src/Data/Unit.purs"
                    , "/src/Data/Monoid.purs"
                    , "/src/Data/Bounded.purs"
                    , "/src/Prelude.purs"
                    , "/src/Data/Monoid/Disj.purs"
                    , "/src/Data/Monoid/Conj.purs"
                    , "/src/Data/Monoid/Additive.purs"
                    , "/src/Data/Monoid/Dual.purs"
                    , "/src/Data/Monoid/Endo.purs"
                    , "/src/Data/Monoid/Multiplicative.purs"
                    , "/src/Data/NaturalTransformation.purs"
                    , "/src/Data/Void.purs"
                    , "/src/Data/Eq.purs"
                    , "/src/Data/Boolean.purs"
                    , "/src/Data/HeytingAlgebra.purs"
                    , "/src/Data/Semiring.purs"
                    , "/src/Data/Ring.purs"
                    , "/src/Data/Semigroup.purs"
                    , "/src/Data/Semigroup/Last.purs"
                    , "/src/Data/Semigroup/First.purs"
                    , "/src/Control/Apply.purs"
                    , "/src/Control/Applicative.purs"
                    , "/src/Control/Bind.purs"
                    , "/src/Control/Monad.purs"
                    , "/src/Control/Semigroupoid.purs"
                    , "/src/Control/Category.purs"
                    , "/src/Record/Unsafe.purs"
                    ]
                purescriptEffect =
                  map
                    ("bower_components/purescript-effect" <> _)
                    [ "/src/Effect.purs"
                    ]
                purescriptEffectConsole =
                  map
                    ("bower_components/purescript-console" <> _)
                    [ "/src/Effect/Console.purs"
                    ]
                testModules =
                  (testsDirectory <> "/" <> file) A.:
                  (((testsDirectory <> "/" <> moduleName <> "/") <> _) <$> subModules)
              in
              purescriptPrelude <>
              purescriptEffect <>
              purescriptEffectConsole <>
              testModules
          }

tempMakeFile :: String
tempMakeFile =
  """
RUNTIME_SOURCES = \
	../../../runtime/purescript.c \
	$(shell find ../../../ccan -type f -name '*.c') \
	$(shell find ../../../vendor -type f -name '*.c')

RUNTIME_OBJECTS = $(patsubst %.c,%.o,$(RUNTIME_SOURCES))

LDFLAGS = -lBlocksRuntime -lgc -lm

%.o: %.c
	@echo "Compile" $^
	@clang \
		-fblocks \
		-D 'uthash_malloc=GC_MALLOC'\
		-D 'uthash_free(ptr, sz)=NULL'\
		-D 'vec_realloc=GC_realloc'\
		-D 'vec_free(x)=NULL'\
		-D 'vec_malloc=GC_MALLOC'\
		-Wall \
		-Wno-unused-variable \
		-Wno-unused-value \
		-c \
		-o $@ \
		-I . \
		$(CLANG_FLAGS) \
		$^

sources = $(shell find . -type f -name '*.c')
objects = $(patsubst %.c,%.o,$(sources))
prog: CLANG_FLAGS = -I . -I ../../..
prog: $(RUNTIME_OBJECTS) $(objects)
	@clang \
		$^ \
		$(LDFLAGS) \
		-o a.out
  """

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
