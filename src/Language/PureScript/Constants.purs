module Language.PureScript.Constants where

-- Prim values

undefined :: String
undefined = "undefined"

-- Type Class Dictionary Names

ringInt :: String
ringInt = "ringInt"

dataRing :: String
dataRing = "Data_Ring"

negate :: String
negate = "negate"

dataSemiring :: String
dataSemiring = "Data_Semiring"

semiringNumber :: String
semiringNumber = "semiringNumber"

semiringInt :: String
semiringInt = "semiringInt"

sub :: String
sub = "sub"

mul :: String
mul = "mul"

add :: String
add = "add"

one :: String
one = "one"

zero :: String
zero = "zero"

dataBounded :: String
dataBounded = "Data_Bounded"

boundedBoolean :: String
boundedBoolean = "boundedBoolean"

top :: String
top = "top"

bottom :: String
bottom = "bottom"

semigroupoidFn :: String
semigroupoidFn = "semigroupoidFn"

controlSemigroupoid :: String
controlSemigroupoid = "Control_Semigroupoid"

compose :: String
compose = "compose"

composeFlipped :: String
composeFlipped = "composeFlipped"

discardUnitDictionary :: String
discardUnitDictionary = "discardUnit"

effectUncurried :: String
effectUncurried = "Effect_Uncurried"

effectDictionaries ::
  { applicativeDict :: String
  , bindDict :: String
  , monadDict :: String
  }
effectDictionaries =
  { applicativeDict: "applicativeEffect"
  , bindDict: "bindEffect"
  , monadDict: "monadEffect"
  }

untilE :: String
untilE = "untilE"

whileE :: String
whileE = "whileE"

controlBind :: String
controlBind = "Control_Bind"

bind :: String
bind = "bind"

controlApplicative :: String
controlApplicative = "Control_Applicative"

pure' :: String
pure' = "pure"

discard :: String
discard = "discard"

dataFunction :: String
dataFunction = "Data_Function"

dataArray :: String
dataArray = "Data_Array"

apply :: String
apply = "apply"

applyFlipped :: String
applyFlipped = "applyFlipped"

unsafeIndex :: String
unsafeIndex = "unsafeIndex"

partialUnsafe :: String
partialUnsafe = "Partial_Unsafe"

unsafePartial :: String
unsafePartial = "unsafePartial"

unsafeCoerce :: String
unsafeCoerce = "Unsafe_Coerce"

unsafeCoerceFn :: String
unsafeCoerceFn = "unsafeCoerce"
