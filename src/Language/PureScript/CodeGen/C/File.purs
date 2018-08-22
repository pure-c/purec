module Language.PureScript.CodeGen.C.File
  ( withHeaderGuard
  , toHeader
  , toBody
  , cModuleName
  , nativeMain
  , isMain
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Control.Monad.State (State, evalState, execState)
import Control.Monad.State as State
import CoreFn.Expr as C
import CoreFn.Module as C
import CoreFn.Names as C
import Data.Array as A
import Data.Either (Either(..))
import Data.Foldable (elem)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Traversable (for_, traverse)
import Data.Tuple.Nested ((/\))
import Debug.Trace (traceM)
import Language.PureScript.CodeGen.C.AST (AST)
import Language.PureScript.CodeGen.C.AST as AST
import Language.PureScript.CodeGen.C.AST as Type
import Language.PureScript.CodeGen.C.Common (safeName)
import Language.PureScript.CodeGen.CompileError (CompileError)
import Language.PureScript.CodeGen.Runtime as R

cModuleName
  :: C.ModuleName
  -> String
cModuleName (C.ModuleName pieces) =
  A.intercalate "_" $
    unwrap <$> pieces

withHeaderGuard
  :: C.ModuleName
  -> Array AST
  -> Array AST
withHeaderGuard moduleName asts =
  let
    headerGuard =
      cModuleName moduleName <> "_H"
  in
    A.concat
      [ [ AST.Raw $ "#ifndef " <> headerGuard
        , AST.Raw $ "#define " <> headerGuard
        , AST.Raw ""
        ]
      , asts
      , [ AST.Raw ""
        , AST.Raw $ "#endif // " <> headerGuard
        ]
      ]

toHeader
  :: Array AST
  -> Array AST
toHeader = A.catMaybes <<< map go

  where
  go (AST.VariableIntroduction { name, initialization }) =
    Just $
      AST.VariableIntroduction
        { name
        , type: Type.Pointer (Type.Any [ Type.Const ])
        , qualifiers: []
        , initialization: Nothing
        }
  go _ =
    Nothing

toBody :: Array AST -> Array AST
toBody = A.catMaybes <<< map go
  where
  go :: AST -> Maybe AST
  go (AST.VariableIntroduction { name, initialization: Just initialization }) = do
    case initialization of
      AST.App a xs ->
        Just $
          AST.App
            R._PURS_ANY_THUNK_DECL
            [ AST.Raw name
            , AST.App a xs
            ]
      AST.Lambda { arguments, returnType, body } ->
        Just $
          AST.App
            R._PURS_ANY_THUNK_DECL
            [ AST.Raw name
            , initialization
            ]
      _ ->
        Just $
          AST.VariableIntroduction
            { name
            , type: Type.Pointer (Type.Any [ Type.Const ])
            , qualifiers: []
            , initialization: Just initialization
            }
  go _ = Nothing

-- XXX: should be configurable
isMain :: C.ModuleName -> Boolean
isMain (C.ModuleName [C.ProperName "Main"]) = true
isMain _ = false

nativeMain :: AST
nativeMain =
  AST.Function
    { name: "main"
    , arguments: []
    , returnType: Type.Primitive Type.Int []
    , qualifiers: []
    , body: Just $
        AST.Block
          [ AST.App (AST.Var "GC_INIT") []
          -- , AST.App (AST.Var (safeName "main")) []
          , AST.Return (AST.NumericLiteral (Left 0))
          ]
    }

