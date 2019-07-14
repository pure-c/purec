module Main where

data Unit = Unit
type Effect a = Unit -> a

foreign import sub :: Int -> Int -> Int

main :: Effect Int
main _ = go { a: 100000000 }
  where
  go { a: 0 } = 0
  go x = go (x { a = sub x.a 1 })
