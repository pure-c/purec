module Language.PureScript.CodeGen.SupplyT
  ( class MonadSupply
  , freshId
  , SupplyT
  , runSupplyT
  ) where

import Prelude

import Control.Monad.Except (ExceptT(..))
import Control.Monad.Reader (ReaderT(..), ask, runReaderT)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

class MonadSupply m where
  freshId :: m Int

newtype SupplyT m a =
  SupplyT
    (ReaderT (Ref Int) m a)

derive newtype instance applicativeSupplyT :: Applicative m => Applicative (SupplyT m)
derive newtype instance functorSupplyT :: Functor m => Functor (SupplyT m)
derive newtype instance monadSupplyT :: Monad m => Monad (SupplyT m)
derive newtype instance monadEffectSupplyT :: MonadEffect m => MonadEffect (SupplyT m)
derive newtype instance monadAffSupplyT :: MonadAff m => MonadAff (SupplyT m)

instance monadSupplyExceptT :: (Monad m, MonadSupply m) => MonadSupply (ExceptT e m) where
  freshId = lift freshId

instance monadTransSupplyT :: MonadTrans SupplyT where
  lift = SupplyT <<< lift

instance monadSupplySupplyT :: MonadEffect m => MonadSupply (SupplyT m) where
  freshId = SupplyT $
    ask >>=
      (liftEffect <<< Ref.modify (_ + 1))

runSupplyT :: ∀ m a. MonadEffect m => SupplyT m a -> m a
runSupplyT (SupplyT x) =
  liftEffect (Ref.new 0) >>=
    runReaderT x
