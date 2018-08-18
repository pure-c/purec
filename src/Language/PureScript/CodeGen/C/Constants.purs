module Language.PureScript.CodeGen.C.Constants
  ( __unused
  , _PURS_ANY_THUNK_DECL
  ) where

import Language.PureScript.CodeGen.C.AST (AST)
import Language.PureScript.CodeGen.C.AST as AST

__unused :: String
__unused = "__unused"

_PURS_ANY_THUNK_DECL :: AST
_PURS_ANY_THUNK_DECL = AST.Var "PURS_ANY_THUNK_DECL"
