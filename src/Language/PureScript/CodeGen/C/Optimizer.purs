module Language.PureScript.CodeGen.C.Optimizer
  ( optimize
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Data.Foldable (foldl)
import Language.PureScript.CodeGen.C.AST (AST)
import Language.PureScript.CodeGen.C.Optimizer.Blocks (collapseNestedBlocks, collapseNestedIfs)
import Language.PureScript.CodeGen.C.Optimizer.Inliner (unThunk)
import Language.PureScript.CodeGen.C.Optimizer.Unused (removeCodeAfterReturnStatements, removeUndefinedApp)
import Language.PureScript.CodeGen.CompileError (CompileError)
import Language.PureScript.CodeGen.SupplyT (class MonadSupply)

-- | Apply a series of optimizer passes
optimize
  :: ∀ m
   . Monad m
  => MonadError CompileError m
  => MonadSupply m
  => AST
  -> m AST
optimize =
  untilFixedPoint (pure <<< tidyUp)

  where
  tidyUp :: AST -> AST
  tidyUp =
    foldl (<<<) identity
      [ collapseNestedBlocks
      , collapseNestedIfs
      , removeCodeAfterReturnStatements
      , removeUndefinedApp
      , unThunk
        ]

untilFixedPoint
  :: ∀ m a
   . Monad m
  => Eq a
  => (a -> m a)
  -> a
  -> m a
untilFixedPoint f = go
  where
  go a = f a >>= \a' ->
    if a == a'
      then pure a'
      else go a'
