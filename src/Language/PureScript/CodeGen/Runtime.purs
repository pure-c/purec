module Language.PureScript.CodeGen.Runtime
  ( purs_any_app

  , purs_any_eq_int
  , purs_any_eq_num
  , purs_any_eq_char
  , purs_any_eq_string
  , purs_any_get_cons
  , purs_any_get_record
  , purs_any_get_array

  , purs_any_cont_new
  , purs_any_array_new
  , purs_any_cons_new
  , purs_any_record_new
  , purs_any_int_new
  , purs_any_num_new
  , purs_any_string_new
  , purs_any_char_new

  , _purs_scope_alloc
  , _purs_scope_new
  , _PURS_SCOPE_T
  , _PURS_CONS_VALUES_NEW

    -- code-gen helpers
  , purs_indirect_thunk_new
  , purs_indirect_value_new
  , purs_indirect_value_assign

    -- misc
  , purs_any_fun_t
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
  , purs_any_true
  , purs_any_false
  , void
  ) where

import Prelude

import Data.Either (Either(..))
import Language.PureScript.CodeGen.C.AST (AST)
import Language.PureScript.CodeGen.C.AST as AST
import Language.PureScript.CodeGen.C.AST as Type

void :: Array AST.TypeQualifier -> AST.Type
void = Type.RawType "void"

any'' :: Array AST.TypeQualifier -> AST.Type
any'' xs = Type.Pointer (Type.Any xs)

any' :: AST.Type
any' = Type.Pointer (Type.Any [])

any :: AST.Type
any = Type.Pointer (Type.Any [ Type.Const ])

purs_any_fun_t :: AST.Type
purs_any_fun_t = Type.RawType "purs_any_fun_t" []

purs_cons_t :: String
purs_cons_t = "purs_cons_t"

purs_any_false :: AST
purs_any_false = AST.Var "purs_any_false"

purs_any_true :: AST
purs_any_true = AST.Var "purs_any_true"

purs_any_eq_int :: AST
purs_any_eq_int = AST.Var "purs_any_eq_int"

purs_any_eq_num :: AST
purs_any_eq_num = AST.Var "purs_any_eq_num"

purs_any_eq_char :: AST
purs_any_eq_char = AST.Var "purs_any_eq_char"

purs_any_eq_string :: AST
purs_any_eq_string = AST.Var "purs_any_eq_string"

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

purs_any_cons_new :: AST
purs_any_cons_new = AST.Var "purs_any_cons_new"

purs_any_int_new :: AST
purs_any_int_new = AST.Var "purs_any_int_new"

purs_any_char_new :: AST
purs_any_char_new = AST.Var "purs_any_char_new"

purs_any_num_new :: AST
purs_any_num_new = AST.Var "purs_any_num_new"

purs_any_array_new :: AST
purs_any_array_new = AST.Var "purs_any_array_new"

purs_any_record_new :: AST
purs_any_record_new = AST.Var "purs_any_record_new"

purs_any_cont_new :: AST
purs_any_cont_new = AST.Var "purs_any_cont_new"

purs_any_string_new :: AST
purs_any_string_new = AST.Var "purs_any_string_new"

purs_indirect_thunk_new :: AST
purs_indirect_thunk_new = AST.Var "purs_indirect_thunk_new"

purs_indirect_value_new :: AST
purs_indirect_value_new = AST.Var "purs_indirect_value_new"

purs_indirect_value_assign :: AST
purs_indirect_value_assign = AST.Var "purs_indirect_value_assign"

_PURS_CONS_VALUES_NEW :: AST
_PURS_CONS_VALUES_NEW = AST.Var "PURS_CONS_VALUES_NEW"

_purs_scope_alloc :: AST
_purs_scope_alloc = AST.Var "_purs_scope_alloc"

_purs_scope_new :: AST
_purs_scope_new = AST.Var "_purs_scope_new"

_PURS_SCOPE_T :: AST
_PURS_SCOPE_T = AST.Var "PURS_SCOPE_T"

purs_record_empty :: AST
purs_record_empty = AST.Var "purs_record_empty"

assert :: AST -> String -> AST
assert condAst message =
  AST.App (AST.Var "purs_assert")
    [ condAst
    , AST.StringLiteral "%s"
    , AST.StringLiteral message
    ]

assert' :: String -> AST
assert' = assert $ AST.NumericLiteral (Left 0)
