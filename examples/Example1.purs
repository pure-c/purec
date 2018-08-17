module Example1 where

foo :: Int
foo = 100

const :: ∀ a b. a -> b -> a
const a _ = a

bar :: ∀ a. a -> Int
bar = const foo
