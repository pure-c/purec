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

class Foo a where
  foo :: a -> { bar :: a }

instance fooInt :: Foo Int where
  foo i = { bar: i }

main :: Effect Int
main _ = (foo 0).bar
