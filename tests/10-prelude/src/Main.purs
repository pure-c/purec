module Main where

import Prelude

type Effect a = Unit -> a

chainE :: Effect Int -> Effect Int -> Effect Int
chainE a b = \_ ->
  let x = a unit
  in
   case x of
    0 -> b unit
    n -> n

infixl 5 chainE as >>

pureE :: ∀ a. a -> Effect a
pureE n _ = n

check :: Boolean -> Effect Int
check = pureE <<< if _ then 0 else 1

runE :: ∀ a. Effect a -> a
runE f = f unit

foreign import putStrLn :: String -> Effect Int

main :: Effect Int
main =
  let
    x = pureE 5
    y = pureE 2
    a = pureE $ runE x + runE y
    z = pureE $ runE x * runE y
    b = pureE $ runE x - runE y
    c = pureE $ runE x / runE y
  in
     check (runE x == 5)
  >> check (runE a == 7)
  >> check (runE b == 3)
  >> check (runE c == 2)
  >> check (show ([] :: Array Int) == "[]")
  >> check (show [ 99 ] == "[99]")
  >> check (show [ 1, 2, 3 ] == "[1, 2, 3]")
  >> check (show "" == "\"\"")
  >> check (show "\"" == "\"\"\"")
  >> check (show {} == "{}")
  >> check (show { a: [ 1 ] } == "{ a: [1] }")
  >> check ([ 1, 2, 3 ] == [ 1, 2, 3 ])
  >> check (map show [ 1, 2, 3 ] == [ "1", "2", "3" ])
  >> check (show (map show [ 1, 2, 3 ]) == "[\"1\", \"2\", \"3\"]")
  >> check (map (_ * 3) [ 1, 2, 3 ] == [ 3, 6, 9 ])
  >> check (pure 3 == [ 3 ])
  >> check (map identity ([] :: Array Int) == [])
