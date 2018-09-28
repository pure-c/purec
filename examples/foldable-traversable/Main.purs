module Main where

import Prelude

import Effect (Effect)
import Effect.Console as Console
import Data.Foldable (foldr, foldl)
import Data.FunctorWithIndex (mapDefault)
import Data.Traversable (traverse)

main :: Effect Unit
main = do
  run "foldr" $ foldr (+) 0 [1,2,3,4,5,6,7,8,9]
  run "foldl" $ foldl (+) 0 [1,2,3,4,5,6,7,8,9]
  run "mapDefault" $ mapDefault (\i -> i + 1) [1,2,3,4,5,6,7,8,9]
  run "traverse" =<< traverse pure [1,2,3,4,5,6,7,8,9]
  where
  run :: forall a. Show a => String -> a -> Effect Unit
  run label result =
    Console.log $
      "[" <> label <> "]: " <> show result
