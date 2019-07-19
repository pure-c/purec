module Main where

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

const :: ∀ a. a -> (∀ b. b -> a)
const x _ = x

main :: Effect Int
main =
  let
    f = const [ 0, 1 ]
  in (\_ ->
        case f 0 of
          [ 0, 1 ] -> 0
          _        -> 1)
  >> (\_ ->
        case f 1 of
          [ 0, 1 ] -> 0
          _        -> 1)
  >> (\_ ->
        case f 2 of
          [ 0, 1 ] -> 0
          _        -> 1)
