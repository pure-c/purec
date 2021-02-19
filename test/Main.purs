module Test.Main
  ( main
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Node.Process (lookupEnv) as Process
import Test.Spec.Reporter (specReporter) as Spec
import Test.Spec.Runner (defaultConfig, runSpecT) as Spec
import Test.Upstream (buildUpstreamTestSuite)

main :: Effect Unit
main = launchAff_ do
  only <- liftEffect (Process.lookupEnv "only")
  upstreamSpec <- buildUpstreamTestSuite only
  unwrap $ Spec.runSpecT
    (Spec.defaultConfig { timeout = Nothing })
    [ Spec.specReporter ] $
      upstreamSpec
