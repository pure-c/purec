module Language.PureScript.CodeGen.Runtime
  ( purs_any_app
  , purs_cons_t
  , _PURS_ANY_THUNK_DECL
  , _PURS_ANY_CONS
  ) where

import Language.PureScript.CodeGen.C.AST (AST)
import Language.PureScript.CodeGen.C.AST as AST

purs_cons_t :: String
purs_cons_t = "purs_cons_t"

purs_any_app :: AST
purs_any_app = AST.Var "purs_any_app"

_PURS_ANY_THUNK_DECL :: AST
_PURS_ANY_THUNK_DECL = AST.Var "PURS_ANY_THUNK_DECL"

_PURS_ANY_CONS :: AST
_PURS_ANY_CONS = AST.Var "PURS_ANY_CONS"
