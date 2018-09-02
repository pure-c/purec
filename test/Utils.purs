module Test.Utils
  ( runProc
  ) where

import Prelude

import Data.Array as A
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.AVar as AVar
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception (error)
import Effect.Ref as Ref
import Node.ChildProcess as ChildProcess
import Node.ChildProcess as ChildProcess.Exit
import Node.Encoding (Encoding(..))
import Node.Path (FilePath)
import Node.ReadLine as RL
import Node.Stream as Stream

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

  stderrRef <- liftEffect $ Ref.new ""
  liftEffect $ Stream.onDataString (ChildProcess.stderr process) UTF8 \x ->
    void $ Ref.modify (_ <> x) stderrRef

  liftEffect do
    stdoutIface <- RL.createInterface (ChildProcess.stdout process) mempty
    RL.setLineHandler stdoutIface \line ->
      Console.log line

  v <- AVar.empty
  liftEffect $
    ChildProcess.onExit process case _ of
      ChildProcess.Exit.Normally 0 ->
        launchAff_ $ AVar.tryPut unit v
      ChildProcess.Exit.Normally _ -> do
        stderr <- Ref.read stderrRef
        launchAff_ $ AVar.kill (error stderr) v
      ChildProcess.Exit.BySignal signal ->
        launchAff_ $ AVar.kill (error $ "Received Signal: " <> show signal) v
  AVar.take v
