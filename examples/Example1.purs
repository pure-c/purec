module Example1 where

data Foo
  = Bar
  | Qux

const :: ∀ a b. a -> b -> a
const static _ = static

bar :: ∀ a. a -> Int
bar = const foo

foo :: Int
foo = 100
