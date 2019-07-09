module Language.PureScript.CodeGen.C.File
  ( withHeaderGuard
  , toHeader
  , toBody
  , dottedModuleName
  , cModuleName
  , cModulePath
  , nativeMain
  , isMain
  ) where

import Prelude

import CoreFn.Names (ModuleName(..), ProperName(..)) as C
import Data.Array as A
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Language.PureScript.CodeGen.C.AST (AST)
import Language.PureScript.CodeGen.C.AST as AST
import Language.PureScript.CodeGen.C.AST as Type
import Language.PureScript.CodeGen.Runtime as R

dottedModuleName
  :: C.ModuleName
  -> String
dottedModuleName (C.ModuleName pieces) =
  A.intercalate "." $
    unwrap <$> pieces

cModuleName
  :: C.ModuleName
  -> String
cModuleName (C.ModuleName pieces) =
  A.intercalate "_" $
    unwrap <$> pieces

cModulePath
  :: C.ModuleName
  -> String
cModulePath =
  dottedModuleName

withHeaderGuard
  :: C.ModuleName
  -> Array AST
  -> Array AST
withHeaderGuard moduleName asts =
  let
    headerGuard =
      cModuleName moduleName <> "__GENERATED__H"
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
  go (AST.VariableIntroduction { name }) =
    Just $
      AST.VariableIntroduction
        { name
        , type: Type.Any []
        , qualifiers: []
        , initialization: Nothing
        }
  go _ =
    Nothing

toBody :: Array AST -> Array AST
toBody = A.catMaybes <<< map go
  where
  go :: AST -> Maybe AST
  go x@(AST.Function _) =
    Just x
  go x@(AST.App v _) | v == R._PURS_SCOPE_T =
    Just x
  go (AST.VariableIntroduction { name, type: typ, initialization: Just initialization }) =
    go' initialization
    where
    go' = case _ of
      AST.Cast _ ast@(AST.App f _)
        | f `A.elem`
            [ R.purs_any_cons_new
            , R.purs_any_int_new
            , R.purs_any_num_new
            , R.purs_any_string_new
            , R.purs_any_record_new
            , R.purs_any_array_new
            ]
        -> go' ast
      _ ->
        Just $
          AST.App
            R._PURS_ANY_THUNK_DEF
            [ AST.Raw name
            , initialization
            ]
  go _ = Nothing

-- XXX: should be configurable
isMain :: C.ModuleName -> Boolean
isMain (C.ModuleName [C.ProperName "Main"]) = true
isMain _ = false

nativeMain :: AST -> AST
nativeMain mainVar =
  AST.Function
    { name: Just "main"
    , arguments: []
    , returnType: Type.Primitive Type.Int []
    , qualifiers: []
    , variadic: false
    , body: Just $
        AST.Block
          [ AST.App (AST.Var "GC_INIT") []
          , AST.App
              R.purs_any_app
              [ mainVar
              , R.purs_any_null
              ]
          , AST.Return (AST.NumericLiteral (Left 0))
          ]
    }
