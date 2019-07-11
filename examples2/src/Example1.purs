module Example1 where

data Unit = Unit
-- import data Effect :: Type -> Type
type Effect a = Unit -> a

foreign import putStr :: String -> Effect Int

main :: Effect Int
main =
  let
    x :: Unit -> Int
    x = \_ -> y Unit

    y :: Unit -> Int
    y = \_ -> x Unit

   -- in \ _-> (x Unit).y
  in
   putStr "test" -- (x Unit).y
