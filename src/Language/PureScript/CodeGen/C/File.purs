module Language.PureScript.CodeGen.C.File
  ( withHeaderGuard
  , toHeader
  , toBody
  , cModuleName
  , nativeMain
  , isMain
  ) where

import Prelude

import CoreFn.Names as C
import Data.Array as A
import Data.Either (Either(..))
import Data.Foldable (elem)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Debug.Trace (traceM)
import Language.PureScript.CodeGen.C.AST (AST)
import Language.PureScript.CodeGen.C.AST as AST
import Language.PureScript.CodeGen.C.Common (safeName)
import Language.PureScript.CodeGen.C.AST as Type
import Language.PureScript.CodeGen.Common (runModuleName)

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
toHeader :: Array AST -> Array AST
toHeader = A.catMaybes <<< map go
  where
  go :: AST -> Maybe AST
  go (AST.VariableIntroduction { name, initialization: Just (AST.Lambda lam) }) =
    Just $
      AST.Function
        { name
        , arguments: lam.arguments
        , returnType: lam.returnType
        , qualifiers: []
        , body: Nothing
        }
  go (AST.VariableIntroduction { name, initialization }) =
    Just $
      AST.VariableIntroduction
        { name
        , type: Type.Pointer (Type.Any [])
        , qualifiers: []
        , initialization: Nothing
        }
  go _ =
    Nothing

toBody :: Array AST -> Array AST
toBody = A.catMaybes <<< map go
  where
  go :: AST -> Maybe AST
  go (AST.VariableIntroduction { name, initialization: Just (AST.Lambda lam) }) =
    Just $
      AST.Function
        { name
        , arguments: lam.arguments
        , returnType: lam.returnType
        , qualifiers: []
        , body: Just lam.body
        }
  go (AST.VariableIntroduction { name, initialization }) =
    Just $
      AST.VariableIntroduction
        { name
        , type: Type.Pointer (Type.Any [])
        , qualifiers: []
        , initialization: Nothing
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
