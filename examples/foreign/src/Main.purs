module Main where

data Unit = Unit
type Effect a = Unit -> a

foreign import data Buffer :: Type
foreign import newBuffer :: Effect Buffer
foreign import bufferSize :: Buffer -> Effect Int
foreign import bufferGrow :: Int -> Buffer -> Effect Int

pureE :: ∀ a. a -> Effect a
pureE n _ = n

bindE :: ∀ a b. Effect a -> (a -> Effect b) -> Effect b
bindE a f _ = (f (a Unit)) Unit

infixl 5 bindE as >>=

main :: Effect Int
main =
  newBuffer >>= \buf ->
    bufferSize buf >>= case _ of
      0 ->
        bufferGrow 20 buf >>= \_ ->
          bufferSize buf >>= case _ of
            20 ->
              pureE 0
            _ ->
              pureE 1
      _ ->
        pureE 1
