module Language.PureScript.CodeGen.Runtime
  ( purs_any_app
  , purs_any_eq_int
  , purs_any_eq_float
  , purs_any_get_cons
  , purs_any_get_record
  , purs_any_get_array
  , _PURS_ANY_CONS
  , _PURS_ANY_INT
  , _PURS_ANY_NUMBER
  , _PURS_ANY_STRING
  , _PURS_ANY_STRING_FROM_LIT
  , _PURS_ANY_RECORD
  , _PURS_ANY_ARRAY
  , _PURS_CONS_VALUES_NEW
  , _PURS_CONS_LIT
  , purs_cons_t
  , purs_cons_get_tag
  , purs_vec_new_from_array
  , purs_record_empty
  , purs_record_find_by_key
  , purs_record_new_from_kvps
  , any
  , any'
  , any''
  , assert
  , assert'
  , _PURS_ANY_THUNK_DECL
  , _PURS_ANY_THUNK_DEF
  , _NULL
  , purs_any_true
  , purs_any_false
  ) where

import Prelude

import Data.Either (Either(..))
import Language.PureScript.CodeGen.C.AST (AST)
import Language.PureScript.CodeGen.C.AST as AST
import Language.PureScript.CodeGen.C.AST as Type

any'' :: _ -> AST.Type
any'' xs = Type.Pointer (Type.Any xs)

any' :: AST.Type
any' = Type.Pointer (Type.Any [])

any :: AST.Type
any = Type.Pointer (Type.Any [ Type.Const ])

purs_cons_t :: String
purs_cons_t = "purs_cons_t"

purs_any_eq_float :: AST
purs_any_eq_float = AST.Var "purs_any_eq_float"

purs_any_false :: AST
purs_any_false = AST.Var "purs_any_false"

purs_any_true :: AST
purs_any_true = AST.Var "purs_any_true"

purs_any_eq_int :: AST
purs_any_eq_int = AST.Var "purs_any_eq_int"

purs_any_get_cons :: AST
purs_any_get_cons = AST.Var "purs_any_get_cons"

purs_any_get_record :: AST
purs_any_get_record = AST.Var "purs_any_get_record"

purs_any_get_array :: AST
purs_any_get_array = AST.Var "purs_any_get_array"

purs_cons_get_tag :: AST
purs_cons_get_tag = AST.Var "purs_cons_get_tag"

purs_any_app :: AST
purs_any_app = AST.Var "purs_any_app"

purs_record_new_from_kvps :: AST
purs_record_new_from_kvps = AST.Var "purs_record_new_from_kvps"

purs_record_find_by_key :: AST
purs_record_find_by_key = AST.Var "purs_record_find_by_key"

purs_vec_new_from_array :: AST
purs_vec_new_from_array = AST.Var "purs_vec_new_from_array"

_PURS_ANY_THUNK_DEF :: AST
_PURS_ANY_THUNK_DEF = AST.Var "PURS_ANY_THUNK_DEF"

_PURS_ANY_THUNK_DECL :: AST
_PURS_ANY_THUNK_DECL = AST.Var "PURS_ANY_THUNK_DECL"

_PURS_ANY_CONS :: AST
_PURS_ANY_CONS = AST.Var "PURS_ANY_CONS"

_PURS_ANY_INT :: AST
_PURS_ANY_INT = AST.Var "PURS_ANY_INT"

_PURS_ANY_STRING_FROM_LIT :: AST
_PURS_ANY_STRING_FROM_LIT = AST.Var "PURS_ANY_STRING"

_PURS_ANY_STRING :: AST
_PURS_ANY_STRING = AST.Var "PURS_ANY_STRING"

_PURS_ANY_NUMBER :: AST
_PURS_ANY_NUMBER = AST.Var "PURS_ANY_NUMBER"

_PURS_ANY_ARRAY :: AST
_PURS_ANY_ARRAY = AST.Var "PURS_ANY_ARRAY"

_PURS_ANY_RECORD :: AST
_PURS_ANY_RECORD = AST.Var "PURS_ANY_RECORD"

_PURS_CONS_VALUES_NEW :: AST
_PURS_CONS_VALUES_NEW = AST.Var "PURS_CONS_VALUES_NEW"

_PURS_CONS_LIT :: AST
_PURS_CONS_LIT = AST.Var "PURS_CONS_LIT"

_NULL :: AST
_NULL = AST.Raw "NULL"

purs_record_empty :: AST
purs_record_empty = AST.Var "purs_record_empty"

assert :: AST -> String -> AST
assert condAst message =
  AST.App (AST.Var "purs_assertf")
    [ condAst
    , AST.StringLiteral message
    ]

assert' :: String -> AST
assert' = assert $ AST.NumericLiteral (Left 0)
