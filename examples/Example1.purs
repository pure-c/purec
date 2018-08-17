module Example1 where

foo :: Int
foo = 100

const :: ∀ a b. a -> b -> a
const static _ = static

bar :: ∀ a. a -> Int
bar = const foo
