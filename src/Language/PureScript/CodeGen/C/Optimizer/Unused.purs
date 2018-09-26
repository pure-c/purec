module Language.PureScript.CodeGen.C.Optimizer.Unused
  ( removeCodeAfterReturnStatements
  , removeUndefinedApp
  ) where

import Prelude

import Data.Array as A
import Data.Maybe (fromMaybe)
import Language.PureScript.Constants as Constants
import Language.PureScript.CodeGen.C.AST (AST, everywhere)
import Language.PureScript.CodeGen.C.AST as AST
import Language.PureScript.CodeGen.C.Optimizer.Common (mapBlock)

removeCodeAfterReturnStatements :: AST -> AST
removeCodeAfterReturnStatements = everywhere (mapBlock go)
  where
  go :: Array AST -> Array AST
  go asts = fromMaybe asts ado
    ix <- A.findIndex isReturn asts
    in A.take (ix + 1) asts

  isReturn (AST.Return _) = true
  isReturn _ = false

removeUndefinedApp :: AST -> AST
removeUndefinedApp = everywhere convert
  where
  convert (AST.App fn [AST.Var arg])
    | arg == Constants.undefined =
        AST.App fn []
  convert ast = ast
