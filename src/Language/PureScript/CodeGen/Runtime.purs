module Language.PureScript.CodeGen.Runtime
  ( purs_any_app
  , purs_any_eq_int
  , purs_any_eq_float
  , purs_cons_t
  , _PURS_ANY_THUNK_DECL
  , _PURS_ANY_CONS
  , _PURS_ANY_INT
  , _PURS_ANY_FLOAT
  , _PURS_ANY_STRING
  , _PURS_ANY_RECORD
  , _PURS_CONS_VALUES_NEW
  , any
  ) where

import Language.PureScript.CodeGen.C.AST (AST)
import Language.PureScript.CodeGen.C.AST as AST
import Language.PureScript.CodeGen.C.AST as Type

any :: AST.Type
any = Type.Pointer (Type.Any [ Type.Const ])

purs_cons_t :: String
purs_cons_t = "purs_cons_t"

purs_any_eq_float :: AST
purs_any_eq_float = AST.Var "purs_any_eq_float"

purs_any_eq_int :: AST
purs_any_eq_int = AST.Var "purs_any_eq_int"

purs_any_app :: AST
purs_any_app = AST.Var "purs_any_app"

_PURS_ANY_THUNK_DECL :: AST
_PURS_ANY_THUNK_DECL = AST.Var "PURS_ANY_THUNK_DECL"

_PURS_ANY_CONS :: AST
_PURS_ANY_CONS = AST.Var "PURS_ANY_CONS"

_PURS_ANY_INT :: AST
_PURS_ANY_INT = AST.Var "PURS_ANY_INT"

_PURS_ANY_STRING :: AST
_PURS_ANY_STRING = AST.Var "PURS_ANY_STRING"

_PURS_ANY_FLOAT :: AST
_PURS_ANY_FLOAT = AST.Var "PURS_ANY_FLOAT"

_PURS_ANY_RECORD :: AST
_PURS_ANY_RECORD = AST.Var "PURS_ANY_RECORD"

_PURS_CONS_VALUES_NEW :: AST
_PURS_CONS_VALUES_NEW = AST.Var "PURS_CONS_VALUES_NEW"
