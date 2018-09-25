module Language.PureScript.CodeGen.C.Optimizer.Blocks
  ( collapseNestedBlocks
  ) where

import Prelude

import Language.PureScript.CodeGen.C.AST (AST, everywhere)

-- | Collapse blocks which appear nested directly below another block
collapseNestedBlocks :: AST -> AST
collapseNestedBlocks = everywhere identity

  -- where
  -- collapse :: AST -> AST
  -- collapse (Block ss sts) = Block ss (concatMap go sts)
  -- collapse js = js

  -- go :: AST -> [AST]
  -- go (Block _ sts) = sts
  -- go s = [s]
