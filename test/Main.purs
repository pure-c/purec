module Test.Main
  ( main
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Reporter (specReporter) as Spec
import Test.Spec.Runner (defaultConfig, runSpecT) as Spec
import Test.Upstream (buildUpstreamTestSuite)

main :: Effect Unit
main = launchAff_ do
  upstreamSpec <- buildUpstreamTestSuite
  unwrap $ Spec.runSpecT
    (Spec.defaultConfig { timeout = Just $ 10000.0 # Milliseconds })
    [ Spec.specReporter ] $
      upstreamSpec
