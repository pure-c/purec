module Main where

data Unit = Unit
type Effect a = Unit -> a
data Maybe a = Just a | Nothing

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
    f = const (Just (Just 0))
    g = const Nothing
  in (\_ ->
        case f 0 of
          Just (Just 0) -> 0
          Nothing       -> 1
          _             -> 1)
  >> (\_ ->
        case f 1 of
          Just (Just 0) -> 0
          Nothing       -> 1
          _             -> 1)
  >> (\_ ->
        case g 2 of
          Nothing  -> 0
          Just _   -> 1)
