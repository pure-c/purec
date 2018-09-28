module Main where

import Prelude

import Effect (Effect)
import Effect.Console as Console
import Effect.Ref as Ref

main :: Effect Unit
main = do
  ref <- Ref.new "new"
  run "Ref.new" =<< Ref.read ref
  Ref.modify_ (const "modify_") ref
  run "Ref.modify_" =<< Ref.read ref
  Ref.write "write" ref
  run "Ref.write" =<< Ref.read ref
  where
  run label result =
    Console.log $
      "[" <> label <> "]: " <> show result
