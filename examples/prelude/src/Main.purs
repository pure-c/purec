module Main where

import Prelude

type Effect a = Unit -> a

main :: Effect Int
main _ = 1
