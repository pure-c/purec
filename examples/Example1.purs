module Example1
  ( Foo (..)
  ) where


import Prelude
import Effect

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

zzx :: Int
zzx = 10

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

showThemAll :: String
showThemAll =
  let
    x :: Int
    x = 0 -- absurd (unsafeCoerce unit)
   in
    show $ identity <$> (
      [ show litChar
      , show true
      , show false
      , show $ litChar == litChar
      , "---"
      , "3 < 10 = "        <> show (3 < 10)
      , "3.0 < 10.0 = "    <> show (3.0 < 10.0)
      , "'a' < 'b' = "     <> show ('a' < 'b')
      , "\"a\" < \"b\" = " <> show ("a" < "b")
      , "true < false = "  <> show (true < false)
      , "false < true = "  <> show (false < true)
      , "---"
      , show $ "[ 1, 2 ] < [ 3, 4 ] = "  <> show ([ 1, 2 ] < [ 3, 4 ])
      , "---"
      , show $ true == true
      , show $ true == false
      , show $ false == false
      , show $ false == true
      , "---"
      , show $ true && true
      , show $ true && false
      , show $ false && false
      , show $ false && true
      , "---"
      , show $ true || true
      , show $ true || false
      , show $ false || false
      , show $ false || true
      , "---"
      , show $ not true
      , show $ not false
      , "---"
      , show litString
      , show litInt
      , show litNumber
      , show litRecord
      , show unit
      , show $ B $ B $ E $ D $ B A
      , "---"
      , show $ [ 100, 200 ] == [ 100, 200 ]
      , show $ [ 200, 400 ] == [ 100, 200 ]
      , "---"
      , "2 + 2 = "     <> show (2 + 2)
      , "5 * 2 = "     <> show (5 * 2)
      , "2.0 + 2.0 = " <> show (2.0 + 2.0)
      , "5.0 * 2.0 = " <> show (5.0 * 2.0)
      , "3 - 2 = "     <> show (3 - 2)
      , "5.0 - 2.0 = " <> show (5.0 - 2.0)
      , "--- apply: ---"
      , show $ [(_ * 2)] <*> [ 2 ]
      , "--- euclidean ring: ---"
      , "0.0 / 0.0 = " <> show (0.0 / 0.0)
      , "2.0 / 2.0 = " <> show (2.0 / 2.0)
      , "20 / 5 = " <> show (20 / 5)
      , "2 % 5 = " <> show (2 `mod` 5)
      , "--- bounded: ---"
      , "bottom Char: "   <> show (bottom :: Char)
      , "bottom Int: "    <> show (bottom :: Int)
      , "bottom Number: " <> show (bottom :: Number)
      , "top Char: "   <> show (top :: Char)
      , "top Int: "    <> show (top :: Int)
      , "top Number: " <> show (top :: Number)
      , "--- concat: ---"
      , show $ [] <> [] :: Array Int
      , show $ [] <> [ 3.0 ]
      , show $ [ 2.0 ] <> []
      , show $ [ 2.0 ] <> [ 3.0 ]
      , "--- bind: ---"
      , show $
          ((do
            _ <- pure 10
            pure 20
            ) :: Array Int)
      , "---"
      ] <>
        let
          xs = [ 1, 2, 3 ]
          ys = [ 1, 2 ]
         in [ show xs, show ys, show ys, show (xs <> ys), show xs, show ys ]
      )

foreign import consoleLog :: String -> Effect Unit
foreign import runGC :: Effect Unit

main_2 :: Effect Unit
main_2 = consoleLog
  let
    x = { a: 100, b: 200 }
    y = x { a = 200 }
  in
    case y of
      foo@{ a: 200 } ->
        show { foo }
      _ ->
        show x

main_3 :: Effect Unit
main_3 = go 1 (mkF unit)
  where
  go 100 _ = do
    consoleLog "done!"
  go n f = do
    runGC
    consoleLog $ f n
    runGC
    go (n + 1) f

  mkF _ =
    let
      r _ = { foo: "bar" }
      k = r unit
      g x =
        a <> b <> x <> show k
      f x =
        g (a <> (show $ x * y))
      y = 2
      a = "hallo: "
      b = "welt: "
    in f

main_1 :: Effect Unit
main_1 = go 1
  where
  go 100 = do
    consoleLog "done!"
  go n = do
    runGC
    consoleLog $ "hello world (" <> show n <> ")"
    consoleLog showThemAll
    runGC
    go (n + 1)

foreign import usleep :: Int -> Effect Unit

main :: Effect Unit
main = do
  main_1 *> runGC *> usleep 100
  main_2 *> runGC *> usleep 100
  main_3 *> runGC *> usleep 100
