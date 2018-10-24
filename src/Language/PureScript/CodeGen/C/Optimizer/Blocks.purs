module Language.PureScript.CodeGen.C.Optimizer.Blocks
  ( collapseNestedBlocks
  , collapseNestedIfs
  ) where

import Prelude

import Data.Array as A
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Language.PureScript.CodeGen.C.AST (AST)
import Language.PureScript.CodeGen.C.AST as AST

-- | Collapse blocks which appear nested directly below another block
collapseNestedBlocks :: AST -> AST
collapseNestedBlocks = AST.everywhere collapse
  where
  collapse :: AST -> AST
  collapse (AST.Block sts) = AST.Block (A.concat $ map go sts)
  collapse x = x

  go :: AST -> Array AST
  go (AST.Block sts) = sts
  go s = [s]

collapseNestedIfs :: AST -> AST
collapseNestedIfs = AST.everywhere collapse
  where
  collapse :: AST -> AST
  collapse (AST.IfElse (AST.NumericLiteral (Left 1)) (AST.Block [ast]) _) = ast
  collapse (AST.IfElse cond1 (AST.Block [AST.IfElse cond2 body Nothing]) Nothing) =
      AST.IfElse (AST.Binary AST.And cond1 cond2) body Nothing
  collapse x = x
