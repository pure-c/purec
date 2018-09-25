module Language.PureScript.CodeGen.C.Optimizer.Common
  ( mapBlocks
  ) where

import Language.PureScript.CodeGen.C.AST (AST)
import Language.PureScript.CodeGen.C.AST as AST

mapBlocks :: (Array AST -> Array AST) -> AST -> AST
mapBlocks go (AST.Block sts) = AST.Block (go sts)
mapBlocks _  x = x
