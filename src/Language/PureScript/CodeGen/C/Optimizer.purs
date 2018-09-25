module Language.PureScript.CodeGen.C.Optimizer
  ( optimize
  ) where

import Prelude

import Data.Foldable (foldl)
import Language.PureScript.CodeGen.C.AST (AST)
import Language.PureScript.CodeGen.SupplyT (class MonadSupply)

-- | Apply a series of optimizer passes to simplified JavaScript code
optimize
  :: ∀ m
   . Monad m
  => MonadSupply m
  => AST
  -> m AST
optimize =
  untilFixedPoint (pure <<< tidyUp)

  --   untilFixedPoint (return . tidyUp) . tco . inlineST
  --     =<< untilFixedPoint (return . magicDo')
  --     =<< untilFixedPoint (return . magicDo) js'
  --     =<< untilFixedPoint
            --  (inlineFnComposition <<<
            --   inlineUnsafeCoerce <<<
            --   inlineUnsafePartial <<<
            --   tidyUp <<<
            --   applyAll
            --     [ inlineCommonValues
            --     , inlineCommonOperators
            --     ])
  where
    tidyUp :: AST -> AST
    tidyUp =
      foldl (<<<) identity
        [
        -- , collapseNestedBlocks
        -- , collapseNestedIfs
        -- , removeCodeAfterReturnStatements
        -- , removeUndefinedApp
        -- , unThunk
        -- , etaConvert
        -- , evaluateIifes
        -- , inlineVariables
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
