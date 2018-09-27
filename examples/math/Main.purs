module Main where

import Prelude

import Effect (Effect)
import Effect.Console as Console
import Math as Math

roughlyEq :: Number -> Number -> Boolean
roughlyEq a b = Math.abs (a - b) < 0.1

main :: Effect Unit
main = do
  run "abs"   $ Math.abs (- 100.0) `roughlyEq` 100.0
  run "acos"  $ Math.acos (- 0.5) `roughlyEq` 2.094395102
  run "atan"  $ Math.atan 0.5 `roughlyEq` 0.4636476090008061
  run "asin"  $ Math.asin 0.5 `roughlyEq` 0.5235987755982989
  run "atan2" $ Math.atan2 0.5 2.0 `roughlyEq` 0.24497866312686414

  where
  run label result =
    Console.log $
      "[" <> label <> "]: " <> show result
