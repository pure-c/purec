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

testString :: Effect Int
testString _ =
  let s = "fooBAZ"
   in case s of
    "fooBAZ" -> 0
    _        -> 1

testRecord :: Effect Int
testRecord _ =
  let r = { a: 1, b: 2, c: 0 }
  in
    case r of
      { a: 1, b: 2, c: 0 } ->
        0
      _ ->
        1

testArray :: Effect Int
testArray _ =
  let xs = [ 1, 2, 0, 3 ]
  in
    case xs of
      [_, _, x, _] -> x
      _            -> 1

main :: Effect Int
main =
  testArray >>
  testRecord >>
  testString
