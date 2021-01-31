module Main where

import Prelude

data Unit = Unit
type Effect a = Unit -> a

chain :: Effect Int -> Effect Int -> Effect Int
chain a b = \_ ->
  let x = a Unit
  in
   case x of
    0 -> b Unit
    n -> n

infixl 5 chain as >>

testAnonLetBoundRecFn :: Int -> Int
testAnonLetBoundRecFn n =
  let
    go 0 = 0
    go n' = go (n' - 1)
  in go n

testAnonRecFn :: Int -> Int
testAnonRecFn n = go n
  where
  go 0 = 0
  go n' = go (n' - 1)

testRecFn :: Int -> Int
testRecFn 0 = 0
testRecFn n = testRecFn (n - 1)

testNonTCORecFn :: Int -> Int
testNonTCORecFn 0 = 0
testNonTCORecFn n =
  if n + testNonTCORecFn (n - 1) == 1
     then 0
     else 1

main :: Effect Int
main =
  (\_ -> testRecFn 10) >>
  (\_ -> testAnonRecFn 10) >>
  (\_ -> testAnonLetBoundRecFn 10) >>
  (\_ -> testNonTCORecFn 1)
