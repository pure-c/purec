module Main where

import Prelude

import Effect (Effect)
import Effect.Console as Console
import Data.Foldable (foldr, foldl)
import Data.FunctorWithIndex (mapDefault)
import Data.Traversable (traverse)

run :: forall a. Show a => String -> a -> Effect Unit
run label result = Console.log $ "[" <> label <> "]: " <> show result

oneTest :: Array Int -> Effect Unit
oneTest arr = do
  Console.log $ "test with input: " <> show arr
  run "foldr" $ foldr (+) 0 arr
  run "foldl" $ foldl (+) 0 arr
  run "mapDefault" $ mapDefault (\i -> i + 1) arr
  run "traverse" =<< traverse pure arr


main :: Effect Unit
main = do
  oneTest []
  oneTest [1]
  oneTest [1, 2]
  oneTest [1, 2, 3]
  oneTest [1, 2, 3, 4]
  oneTest [1, 2, 3, 4, 5]
  oneTest [1, 2, 3, 4, 5, 6]
  oneTest [1, 2, 3, 4, 5, 6, 7]
  oneTest [1, 2, 3, 4, 5, 6, 7, 8]
  oneTest [1, 2, 3, 4, 5, 6, 7, 8, 9]
