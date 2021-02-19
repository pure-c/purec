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

testAnonLetBoundRecFn :: Int -> Effect Int
testAnonLetBoundRecFn n = \_ ->
  let
    go 0 = 0
    go n' = go (n' - 1)
  in go n

testAnonRecFn :: Int -> Effect Int
testAnonRecFn n = \_ -> go n
  where
  go 0 = 0
  go n' = go (n' - 1)

testRecFn :: Int -> Effect Int
testRecFn 0 = \_ -> 0
testRecFn n = \_ -> testRecFn (n - 1) Unit

testNonTCORecFn :: Int -> Effect Int
testNonTCORecFn 0 = \_ -> 0
testNonTCORecFn n = \_ ->
  if n + (testNonTCORecFn (n - 1) Unit) == 1
     then 0
     else 1

testLetBoundRecFn2 :: Effect Int
testLetBoundRecFn2 = \_ -> loop 0
  where
  loop 0 = 0
  loop n =
    let x = loop 0
     in x

testLetBoundRecFn3 :: Effect Int
testLetBoundRecFn3 = \_ -> loop 0
  where
  loop 0 = 0
  loop n =
    let loop _ = 0 -- shadow 'loop' here on purpose
        x = loop 0
     in x

testApplyN :: (Int -> Int) -> Int -> Int -> Int
testApplyN f = go
  where
  go n acc
    | n <= 0 = acc
    | otherwise = go (n - 1) (f acc)

testApplyN2 :: forall a. Int -> (a -> a) -> a -> a
testApplyN2 n f x
  | n <= 0    = x
  | otherwise = testApplyN2 (n - 1) f (f x)

main :: Effect Int
main =
  (testRecFn 1) >>
  (testAnonRecFn 10) >>
  (testAnonLetBoundRecFn 10) >>
  (testNonTCORecFn 1) >>
  testLetBoundRecFn2 >>
  testLetBoundRecFn3 >>
  (\_ -> testApplyN (\_ -> 0) 10000 0) >>
  (\_ -> testApplyN2 10000 (\_ -> 0) 0)
