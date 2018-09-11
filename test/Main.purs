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
      -- A.dropWhile (\test -> test.name /= "2288") <$>
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
                -- compile each module to it's corefn json rep
                Console.log "Compiling PureScript to CoreFn..."
                runProc "purs" $
                  [ "compile"
                  , "-o", outputDir
                  , "-g", "corefn"
                  ] <> test.files

                -- compile each module's corefn json rep to c
                Console.log "Compiling CoreFn to C..."
                void $
                  FS.readdir outputDir >>= traverse \moduleName ->
                    let
                      corefnJsonFile =
                        outputDir <> "/" <> moduleName <> "/corefn.json"
                    in do
                      whenM (FS.exists corefnJsonFile) do
                        Console.log $ "Compiling to C: " <> moduleName <> "..."
                        Main.compileModule
                          (const $ moduleName == test.name || moduleName == "Main")
                          corefnJsonFile

                -- generate Makefile
                FS.writeTextFile UTF8 (outputDir <> "/Makefile") tempMakeFile

                -- build the project using Makefile (will produce an executable
                -- called 'a.out'
                runProc "make"
                  [ "-j", "8"
                  , "-C", outputDir
                  ]

                -- run the built executable
                runProc (outputDir <> "/a.out") []

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
                purescriptAssert =
                  map
                    ("bower_components/purescript-assert" <> _)
                    [ "/src/Test/Assert.purs"
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
              purescriptAssert <>
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

foreign import errorCodeImpl
  :: ∀ a
   . Maybe a
  -> (a -> Maybe a)
  -> Error
  -> Maybe String

errorCode :: Error -> Maybe String
errorCode = errorCodeImpl Nothing Just
