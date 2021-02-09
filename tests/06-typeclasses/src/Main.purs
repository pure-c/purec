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

class FakeMonoid m where
  mempty :: m

instance arrayFakeMonoid :: FakeMonoid (Array a) where
  mempty = []

class Foo a where
  foo :: a -> { bar :: a }
  bar :: ∀ m. FakeMonoid m => a -> m

instance fooInt :: Foo Int where
  foo i = { bar: i }
  bar _ = mempty

main :: Effect Int
main =
  (\_ -> (foo 0).bar) >>
  (\_ -> case bar 0 of
      [] -> 0
      _  -> 1)
