module Example1 where

import Prelude
import Effect

data Maybe a
  = Just a
  | Nothing

foreign import putStr :: String -> Effect Unit
foreign import putStrLn :: String -> Effect Unit
foreign import getLineImpl :: ∀ a. (a -> Maybe a) -> Maybe a -> Effect (Maybe String)
foreign import exit :: ∀ a. Int -> Effect a

getLine :: Effect (Maybe String)
getLine = getLineImpl Just Nothing

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
  getName = do
    putStr "name> "
    getLine >>= case _ of
      Nothing ->
        exit 0
      Just "" -> do
        putStrLn "Don't be silly now, give us your name."
        getName
      Just line ->
        pure line
