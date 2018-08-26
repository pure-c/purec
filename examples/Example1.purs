module Example1
  ( Foo (..)
  ) where

import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Show
import Type.Data.RowList (RLProxy(..))

data Maybe a
  = Nothing
  | Just a

x :: Boolean -> Foo
x true = Qux
x false = Bar 0 "hi"

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

-- XXX: will be replaced by Monoid instance for strings
foreign import concatStringImpl :: String -> String -> String

zzx :: Int
zzx = 10

xs :: Array String
xs = mapArrayImpl show [ 10 ]

foreign import mapArrayImpl :: ∀ a b. (a -> b) -> Array a -> Array b

instance showFoo :: Show Foo where
  show (Bar n x) =
    concatStringImpl "(Bar " (
      concatStringImpl (show n) (
        concatStringImpl " " (
           concatStringImpl (show x) ")")))
  show Qux = "(Qux)"

main' = show xs
