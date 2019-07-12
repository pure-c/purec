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

testString :: Effect Int
testString =
  let s = "fooBAZ"
   in \_ ->
    case s of
      "fooBAZ" -> 0
      _        -> 1

testRecord :: Effect Int
testRecord =
  let r = { a: 1, b: 2, c: 0 }
  in \_ ->
    case r of
      { a: 1, b: 2, c: 0 } ->
        0
      _ ->
        1

testArray :: Effect Int
testArray =
  let xs = [ 1, 2, 0, 3 ]
  in \_ ->
    case xs of
      [_, _, x, _] -> x
      _            -> 1

uselessArrayIndex :: ∀ a. Array a -> Int -> Maybe a
uselessArrayIndex xs 0 =
  case xs of
    [x, _] -> Just x
    _   -> Nothing
uselessArrayIndex xs 1 =
  case xs of
    [_, x] -> Just x
    _      -> Nothing
uselessArrayIndex _ _ =
  Nothing

twice :: Effect Int -> Effect Int
twice x = x >> x

main :: Effect Int
main =
  let
    f = uselessArrayIndex [ 0, 1 ]
  in
    twice (testArray >> testRecord >> testString)
      >> (\_ ->
            case f 0 of
              Just 0  -> 0
              _       -> 1)
      >> (\_ ->
            case f 1 of
              Just 1  -> 0
              _       -> 1)
      >> (\_ ->
            case f 2 of
              Nothing -> 0
              _       -> 1)
