module Test.Main
  ( main
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Test.Spec.Reporter (specReporter) as Spec
import Test.Spec.Runner (defaultConfig, run') as Spec
import Test.Upstream (buildUpstreamTestSuite)

main :: Effect Unit
main = launchAff_ do
  upstreamSpec <- buildUpstreamTestSuite
  liftEffect $
    Spec.run' (Spec.defaultConfig { timeout = Just 10000 }) [Spec.specReporter] $
      upstreamSpec
