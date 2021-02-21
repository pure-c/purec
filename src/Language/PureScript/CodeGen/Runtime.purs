module Language.PureScript.CodeGen.Runtime
  (

    -- any: dynamic runtime types
    any

    -- any: built-ins
  , purs_any_app
  , purs_any_eq_int
  , purs_any_eq_num
  , purs_any_eq_char
  , purs_any_eq_string
  , purs_any_unsafe_get_cons
  , purs_any_unsafe_get_int
  , purs_any_unsafe_get_foreign
  , purs_any_unsafe_get_record
  , purs_any_unsafe_get_array
  , purs_any_force_cons
  , purs_any_force_int
  , purs_any_force_array
  , purs_any_force_record
  , purs_any_true
  , purs_any_false
  , purs_any_null
  , purs_any_int_zero
  , purs_any_num_zero
  , purs_any_int_one
  , purs_any_num_one

    -- any: built-ins (added for code-gen)
  , purs_any_int_neg

    -- any: initializors
  , purs_any_cont
  , purs_any_array
  , purs_any_cons
  , purs_any_record
  , purs_any_int
  , purs_any_foreign
  , purs_any_num
  , purs_any_string
  , purs_any_char

    -- code-gen helpers
  , purs_any_lazy_new
  , purs_any_ref_new
  , purs_any_ref_write
  , purs_malloc_any_buf
  , purs_address_of
  , purs_derefence

    -- cont
  , purs_cont_t

    -- scope
  , purs_scope_t
  , purs_scope_new
  , purs_scope_new1

  , _PURS_ANY_THUNK_DECL
  , _PURS_ANY_THUNK_DEF

    -- foreign
  , purs_foreign_new

    -- data constructors
  , purs_cons_t
  , purs_cons_new

    -- arrays
  , purs_vec_new_va

    -- continuations
  , purs_cont_new

    -- strings
  , purs_str_new
  , purs_str_static_new

    -- records
  , purs_record_t
  , purs_any_record_empty
  , purs_record_find_by_key
  , purs_record_add_multi
  , purs_record_new_va

    -- misc
  , purs_assert
  , purs_assert'

    -- ...
  , void
  , int
  ) where

import Prelude

import Data.Either (Either(..))
import Language.PureScript.CodeGen.C.AST (AST)
import Language.PureScript.CodeGen.C.AST as AST
import Language.PureScript.CodeGen.C.AST as Type

int :: Array AST.TypeQualifier -> AST.Type
int = Type.RawType "int"

void :: Array AST.TypeQualifier -> AST.Type
void = Type.RawType "void"

any :: AST.Type
any = Type.Any []

purs_record_t :: String
purs_record_t = "purs_record_t"

purs_cont_t :: String
purs_cont_t = "purs_cont_t"

purs_scope_t :: String
purs_scope_t = "purs_scope_t"

purs_cons_t :: String
purs_cons_t = "purs_cons_t"

purs_cons_new :: AST
purs_cons_new = AST.Var "purs_cons_new"

purs_any_num_one :: AST
purs_any_num_one = AST.Var "purs_any_num_one"

purs_any_int_one :: AST
purs_any_int_one = AST.Var "purs_any_int_one"

purs_any_num_zero :: AST
purs_any_num_zero = AST.Var "purs_any_num_zero"

purs_any_int_zero :: AST
purs_any_int_zero = AST.Var "purs_any_int_zero"

purs_any_null :: AST
purs_any_null = AST.Var "purs_any_null"

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

purs_any_unsafe_get_foreign :: AST
purs_any_unsafe_get_foreign = AST.Var "purs_any_unsafe_get_foreign"

purs_any_unsafe_get_int :: AST
purs_any_unsafe_get_int = AST.Var "purs_any_unsafe_get_int"

purs_any_unsafe_get_cons :: AST
purs_any_unsafe_get_cons = AST.Var "purs_any_unsafe_get_cons"

purs_any_unsafe_get_record :: AST
purs_any_unsafe_get_record = AST.Var "purs_any_unsafe_get_record"

purs_any_force_cons :: AST
purs_any_force_cons = AST.Var "purs_any_force_cons"

purs_any_force_int :: AST
purs_any_force_int = AST.Var "purs_any_force_int"

purs_any_force_array :: AST
purs_any_force_array = AST.Var "purs_any_force_array"

purs_any_force_record :: AST
purs_any_force_record = AST.Var "purs_any_force_record"

purs_any_unsafe_get_array :: AST
purs_any_unsafe_get_array = AST.Var "purs_any_unsafe_get_array"

purs_any_app :: AST
purs_any_app = AST.Var "purs_any_app"

purs_record_new_va :: AST
purs_record_new_va = AST.Var "purs_record_new_va"

purs_record_find_by_key :: AST
purs_record_find_by_key = AST.Var "purs_record_find_by_key"

purs_record_add_multi :: AST
purs_record_add_multi = AST.Var "purs_record_add_multi"

purs_vec_new_va :: AST
purs_vec_new_va = AST.Var "purs_vec_new_va"

purs_cont_new :: AST
purs_cont_new = AST.Var "purs_cont_new"

purs_str_new :: AST
purs_str_new = AST.Var "purs_str_new"

purs_str_static_new :: AST
purs_str_static_new = AST.Var "purs_str_static_new"

purs_scope_new1 :: AST
purs_scope_new1 = AST.Var "purs_scope_new1"

purs_scope_new :: AST
purs_scope_new = AST.Var "purs_scope_new"

_PURS_ANY_THUNK_DEF :: AST
_PURS_ANY_THUNK_DEF = AST.Var "PURS_ANY_THUNK_DEF"

_PURS_ANY_THUNK_DECL :: AST
_PURS_ANY_THUNK_DECL = AST.Var "PURS_ANY_THUNK_DECL"

purs_any_cons :: AST
purs_any_cons = AST.Var "purs_any_cons"

purs_any_foreign :: AST
purs_any_foreign = AST.Var "purs_any_foreign"

purs_foreign_new :: AST
purs_foreign_new = AST.Var "purs_foreign_new"

purs_any_int :: AST
purs_any_int = AST.Var "purs_any_int"

purs_any_char :: AST
purs_any_char = AST.Var "purs_any_char"

purs_any_num :: AST
purs_any_num = AST.Var "purs_any_num"

purs_any_array :: AST
purs_any_array = AST.Var "purs_any_array"

purs_any_record :: AST
purs_any_record = AST.Var "purs_any_record"

purs_any_cont :: AST
purs_any_cont = AST.Var "purs_any_cont"

purs_any_string :: AST
purs_any_string = AST.Var "purs_any_string"

purs_any_lazy_new :: AST
purs_any_lazy_new = AST.Var "purs_any_lazy_new"

purs_any_ref_new :: AST
purs_any_ref_new = AST.Var "purs_any_ref_new"

purs_any_ref_write :: AST
purs_any_ref_write = AST.Var "purs_any_ref_write"

purs_malloc_any_buf :: AST
purs_malloc_any_buf = AST.Var "purs_malloc_any_buf"

purs_address_of :: AST
purs_address_of = AST.Var "purs_address_of"

purs_derefence :: AST
purs_derefence = AST.Var "purs_derefence"

purs_any_record_empty :: AST
purs_any_record_empty = AST.Var "purs_any_record_empty"

purs_assert :: AST -> String -> AST
purs_assert condAst message =
  AST.App (AST.Var "purs_assert")
    [ condAst
    , AST.StringLiteral "%s"
    , AST.StringLiteral message
    ]

purs_assert' :: String -> AST
purs_assert' = purs_assert $ AST.NumericLiteral (Left 0)

purs_any_int_neg :: AST
purs_any_int_neg = AST.Var "purs_any_int_neg"
