module Language.PureScript.CodeGen.Runtime
  ( purs_any_app
  ) where

import Language.PureScript.CodeGen.C.AST (AST)
import Language.PureScript.CodeGen.C.AST as AST

purs_any_app :: AST
purs_any_app = AST.Var "purs_any_app"
