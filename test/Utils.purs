module Test.Utils
  ( runProc
  , mkdirp
  , errorCode
  ) where

import Prelude

import Control.Monad.Error.Class (catchError, throwError)
import Data.Array as A
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (wrap)
import Data.String as Str
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.AVar as AVar
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception (Error, error)
import Node.ChildProcess as ChildProcess
import Node.ChildProcess as ChildProcess.Exit
import Node.FS.Aff as FS
import Node.ReadLine as RL

-- | Run a process for it's side-effect
runProc
  :: String
  -> Array String
  -> Aff Unit
runProc cmd args = do
  process <-
    liftEffect $
      ChildProcess.spawn cmd args $
        ChildProcess.defaultSpawnOptions

  liftEffect do
    iface <- RL.createInterface (ChildProcess.stderr process) mempty
    RL.setLineHandler iface \line ->
      Console.error line

  liftEffect do
    iface <- RL.createInterface (ChildProcess.stdout process) mempty
    RL.setLineHandler iface \line ->
      Console.log line

  v <- AVar.empty
  liftEffect $
    ChildProcess.onExit process case _ of
      ChildProcess.Exit.Normally 0 ->
        launchAff_ $ AVar.tryPut unit v
      ChildProcess.Exit.Normally n -> do
        launchAff_ $ AVar.kill (error $ "Subcommand exited with: " <> show n) v
      ChildProcess.Exit.BySignal signal ->
        launchAff_ $ AVar.kill (error $ "Received Signal: " <> show signal) v
  AVar.take v

foreign import errorCodeImpl
  :: ∀ a
   . Maybe a
  -> (a -> Maybe a)
  -> Error
  -> Maybe String

errorCode :: Error -> Maybe String
errorCode = errorCodeImpl Nothing Just

-- TODO Pick up a library for this
mkdirp :: String -> Aff Unit
mkdirp dir = go Nothing (Str.split (wrap "/") dir)
  where
  go cd xs
    | Just { head: x, tail: xs' } <- A.uncons xs
    =
      let
        cd' =
          maybe "" (_ <> "/") cd <> x
      in do
        unless (Str.null cd') do
          mkdir cd'
          go (Just cd') xs'
  go _ _ =
    pure unit

  mkdir dir' =
    FS.mkdir dir' `catchError` \e ->
      unless (errorCode e == Just "EEXIST") do
        throwError e
