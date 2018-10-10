module Language.PureScript.CodeGen.C.Optimizer
  ( optimize
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Data.Foldable (foldl)
import Language.PureScript.CodeGen.C.AST (AST)
import Language.PureScript.CodeGen.C.Optimizer.Blocks (collapseNestedBlocks, collapseNestedIfs)
import Language.PureScript.CodeGen.C.Optimizer.Inliner (etaConvert, inlineCommonValues, inlineFnComposition, inlineVariables, unThunk)
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
  applyAll
    [ untilFixedPoint $
        applyAll
          [ pure <<< inlineCommonValues
          ]
    , untilFixedPoint $
        applyAll
          [ pure <<< collapseNestedBlocks
          , pure <<< collapseNestedIfs
          , pure <<< removeCodeAfterReturnStatements
          , pure <<< removeUndefinedApp
          , pure <<< unThunk
          , etaConvert
          , inlineVariables
          , inlineFnComposition
          ]
    ]

  where
  applyAll =
    foldl (<=<) (pure <<< identity)

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
