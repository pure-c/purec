module Example1 where

import Effect
import Prelude

import Effect.Uncurried (EffectFn2, runEffectFn2)

data Maybe a
  = Just a
  | Nothing

foreign import putStr :: String -> Effect Unit
foreign import putStrLn :: String -> Effect Unit
foreign import getLineImpl
  :: ∀ a
   . EffectFn2
      (a -> Maybe a)
      (Maybe a)
      (Maybe String)
foreign import exit :: ∀ a. Int -> Effect a

getLine :: Effect (Maybe String)
getLine = runEffectFn2 getLineImpl Just Nothing

instance showMaybe :: Show a => Show (Maybe a) where
  show Nothing = "(Nothing)"
  show (Just a) = "(Just " <> show a <> ")"

type User =
  { name :: String
  , email :: String
  }

main :: Effect Unit
main = do
  putStrLn "Please enter your details:"
  name <- getName
  putStrLn $ "Hello, " <> name <> "!"

  where
  getFoo = do
    putStr "name> "
    let
      x =
        let
          getFoo = "1000"
        in getFoo <> "."
    getLine >>= case _ of
      Nothing ->
        exit 0
      Just "" -> do
        putStrLn $ "Please, hand me your name: " <> x
        getName
      Just line ->
        pure line
  getName = do
    putStr "name> "
    getLine >>= case _ of
      Nothing ->
        exit 0
      Just "" -> do
        putStrLn "Don't be silly now, give us your name."
        getFoo
      Just line ->
        pure line
