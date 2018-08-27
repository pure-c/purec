module Example1
  ( Foo (..)
  ) where

import Data.Show (class Show, show)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Unit (unit)
import Data.Void (Void, absurd)
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

litString :: String
litString = "foobår"

litChar :: Char
litChar = 'å'

litInt :: Int
litInt = 10

litNumber :: Number
litNumber = 100.0

litRecord :: { b :: String, a :: Int }
litRecord = { b: "hi!", a: 42 }

foreign import unsafeCoerce :: ∀ a b. a -> b

data A = A | B A | E C
data C = C | D A

instance showC :: Show C where
  show C = "(C)"
  show (D d) = (concatStringImpl "(D " (concatStringImpl (show d) ")"))


instance showA :: Show A where
  show A = "(A)"
  show (B a) = (concatStringImpl "(B " (concatStringImpl (show a) ")"))
  show (E c) = (concatStringImpl "(C " (concatStringImpl (show c) ")"))

main' =
  let
    x :: Int
    x = 0 -- absurd (unsafeCoerce unit)
   in
    show
      [ show litChar
      , show litString
      , show litInt
      , show litNumber
      , show litRecord
      , show unit
      , show (B (B (E (D (B A)))))
      ]
