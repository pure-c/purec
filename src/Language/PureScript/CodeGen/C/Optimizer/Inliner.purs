module Language.PureScript.CodeGen.C.Optimizer.Inliner
  ( unThunk
  ) where

import Prelude

import Data.Array as A
import Data.Maybe (Maybe(..))
import Language.PureScript.CodeGen.C.AST (AST, everywhere)
import Language.PureScript.CodeGen.C.AST as AST

unThunk :: AST -> AST
unThunk = everywhere convert
  where
  convert :: AST -> AST
  convert block@(AST.Block []) = block
  convert block@(AST.Block asts) =
    case A.unsnoc asts of
      Just
        { last:
             AST.Return
              (AST.App
                (AST.Lambda
                  { arguments: [], body: AST.Block body })
                [])
        , init
        } ->
        AST.Block $ init <> body
      _ ->
        block
  convert ast = ast
