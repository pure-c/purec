module POSIX.GetOpt.Monad
  ( GetOptT
  , runGetOptT
  , getopt
  , argv
  , optind
  ) where

import Prelude

import Control.Monad.Reader (ReaderT(..), ask, runReaderT)
import Data.Traversable (for_)
import Effect.Class (class MonadEffect, liftEffect)
import Node.Process as Process
import POSIX.GetOpt as GetOpt

type GetOptT =
  ReaderT GetOpt.BasicParser

runGetOptT
  :: ∀ m a
   . MonadEffect m
  => GetOpt.OptString
  -> GetOpt.Argv
  -> GetOpt.OptInd
  -> GetOptT m a
  -> m a
runGetOptT optstring argv optind action =
  runReaderT action =<< do
    liftEffect $ GetOpt.newBasicParser optstring argv optind

getopt
  :: ∀ m
   . MonadEffect m
  => GetOptT m GetOpt.GetOptRet
getopt = do
  parser <- ask
  mOpt    <- liftEffect $ GetOpt.getopt parser
  mOpt <$ do
    for_ mOpt \{ error } ->
      when error do
        liftEffect $ Process.exit 1

optind
  :: ∀ m
   . MonadEffect m
  => GetOptT m GetOpt.OptInd
optind = do
  parser <- ask
  liftEffect $ GetOpt.optind parser

argv
  :: ∀ m
   . MonadEffect m
  => GetOptT m GetOpt.Argv
argv = do
  parser <- ask
  liftEffect $ GetOpt.argv parser
