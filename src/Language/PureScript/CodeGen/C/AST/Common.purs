module Language.PureScript.CodeGen.C.AST.Common
  ( isRerefenced
  ) where

import Prelude

import Language.PureScript.CodeGen.C.AST (AST)
import Language.PureScript.CodeGen.C.AST as AST

data IsUsed
  = Used
  | Shadowed
  | Unused

derive instance eqIsUsed :: Eq IsUsed

combine :: IsUsed -> IsUsed -> IsUsed
combine Used _     = Used
combine Unused x   = x
combine Shadowed _ = Shadowed

isRerefenced :: String -> AST -> Boolean
isRerefenced needle ast =
  AST.everything combine go ast /= Used
  where
  go (AST.VariableIntroduction { name }) | name == needle = Shadowed
  go (AST.Assignment (AST.Var name) _) | name == needle = Shadowed
  go (AST.Var name) | name == needle = Used
  go _ = Unused
