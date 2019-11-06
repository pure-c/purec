module Main where

import Prelude
import Effect
import Effect.Unsafe

foreign import putStrLn :: String -> Effect Int
foreign import someStr :: Effect String

main :: Effect Int
main =
  let message = unsafePerformEffect someStr
   in bind (void (pure { foo: "bar" })) \_ ->
        bind someStr \s ->
          bind (putStrLn s) \_ ->
            bind someStr \s' ->
              bind (putStrLn (s <> "and" <> s')) \_ ->
                putStrLn "world"
