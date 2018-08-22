module Example1
  ( Foo (..)
  ) where

import Type.Data.RowList (RLProxy(..))

data Foo
  = Bar Int String
  | Qux

const :: ∀ a b. a -> b -> a
const static _ = static

bar :: ∀ a. a -> Int
bar = const foo

eq' :: Int -> Int -> Boolean
eq' 200 200 | false = true
eq' 100 100 = true
eq' _ _ = false

foo :: Int
foo =
  let
    y = 200
    z = 200
    y' = 200
  in
    let z = 200
     in 100

class Show a where
  show :: a -> String

foreign import showIntImpl :: Int -> String
foreign import showStringImpl :: String -> String

-- XXX: will be replaced by Monoid instance for strings
foreign import concatStringImpl :: String -> String -> String

instance showInt :: Show Int where
  show = showIntImpl

instance showString :: Show String where
  show = showStringImpl


instance showFoo :: Show Foo where
  show (Bar n x) =
    concatStringImpl "(Bar " (
      concatStringImpl (show n) (
        concatStringImpl " " (
           concatStringImpl (show x) ")")))
  show Qux = "(Qux)"

-- instance functorFoo :: Functor Foo where
--   map _ (Bar x y) = Bar x y
--   map _ Qux = Qux

main' = show (Bar 100 "h\"ello")
