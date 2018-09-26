module Language.PureScript.CodeGen.C.AST
  ( UnaryOperator(..)
  , BinaryOperator(..)
  , AST(..)
  , Type(..)
  , PrimitiveType(..)
  , TypeQualifier(..)
  , ValueQualifier
  , everywhere
  ) where

import Prelude

import Data.Array (foldl)
import Data.Array as A
import Data.Bifunctor (bimap)
import Data.Either (Either)
import Data.Foldable (foldl)
import Data.Generic.Rep as Rep
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\))
import Foreign.Object (Object)

-- | Built-in unary operators
data UnaryOperator
  -- | Numeric negation
  = Negate
  -- | Boolean negation
  | Not
  -- | Bitwise negation
  | BitwiseNot
  -- | Numeric unary 'plus'
  | Positive
  -- | Get size
  | Size
  -- | Check if empty
  | Empty

derive instance genericUnaryOperator :: Rep.Generic UnaryOperator _

instance eqUnaryOperator :: Eq UnaryOperator where
  eq = genericEq

instance showUnaryOperator :: Show UnaryOperator where
  show = genericShow

-- | Built-in binary operators
data BinaryOperator
  -- | Numeric addition
  = Add
  -- | Numeric subtraction
  | Subtract
  -- | Numeric multiplication
  | Multiply
  -- | Numeric division
  | Divide
  -- | Remainder
  | Modulus
  -- | Generic equality test
  | EqualTo
  -- | Generic inequality test
  | NotEqualTo
  -- | Numeric less-than
  | LessThan
  -- | Numeric less-than-or-equal
  | LessThanOrEqualTo
  -- | Numeric greater-than
  | GreaterThan
  -- | Numeric greater-than-or-equal
  | GreaterThanOrEqualTo
  -- | Boolean and
  | And
  -- | Boolean or
  | Or
  -- | Bitwise and
  | BitwiseAnd
  -- | Bitwise or
  | BitwiseOr
  -- | Bitwise xor
  | BitwiseXor
  -- | Bitwise left shift
  | ShiftLeft
  -- | Bitwise right shift
  | ShiftRight

derive instance genericBinaryOperator :: Rep.Generic BinaryOperator _

instance eqBinaryOperator :: Eq BinaryOperator where
  eq = genericEq

instance showBinaryOperator :: Show BinaryOperator where
  show = genericShow

data ValueQualifier
  = Ref

derive instance genericValueQual :: Rep.Generic ValueQualifier _

instance eqValueQual :: Eq ValueQualifier where
  eq = genericEq

instance showValueQual :: Show ValueQualifier where
  show = genericShow

data TypeQualifier
  = Const
  | BlockStorage

derive instance genericTypeQualifier :: Rep.Generic TypeQualifier _

instance eqTypeQualifier :: Eq TypeQualifier where
  eq = genericEq

instance showTypeQualifier :: Show TypeQualifier where
  show = genericShow

data PrimitiveType
  = Int
  | Void

derive instance genericPrimitiveType :: Rep.Generic PrimitiveType _

instance eqPrimitiveType :: Eq PrimitiveType where
  eq = genericEq

instance showPrimitiveType :: Show PrimitiveType where
  show = genericShow

data Type
  = Pointer Type
  | Any (Array TypeQualifier)
  | RawType String (Array TypeQualifier)
  | Primitive PrimitiveType (Array TypeQualifier)

derive instance genericType :: Rep.Generic Type _

instance eqType :: Eq Type where
  eq x = genericEq x

instance showType :: Show Type where
  show x = genericShow x

-- | Data type for simplified C expressions
data AST
  -- | A numeric literal
  = NumericLiteral (Either Int Number)

  -- | A string literal
  | StringLiteral String

  -- | A character literal
  | CharLiteral Char

  -- | Struct initialization
  | StructLiteral (Object AST)

  -- | A unary operator application
  | Unary UnaryOperator AST

  -- | A binary operator application
  | Binary BinaryOperator AST AST

  -- | An array literal
  | ArrayLiteral (Array AST)

  -- | An enum definition
  | Enum
      { name :: Maybe String
      , members :: Array String
      }

  -- | An array indexer expression
  | Indexer AST AST

  -- | An instance dictionary literal
  | ObjectLiteral (Array { key :: AST, value :: AST })

  -- | An general property accessor expression
  | Accessor AST AST

  -- | A function introduction
  | Function
      { name :: String
      , arguments :: Array { name :: String, type :: Type }
      , returnType :: Type
      , qualifiers :: Array ValueQualifier
      , body :: Maybe AST
      }
  -- | A lambda introduction (via Blocks)
  | Lambda
      { arguments :: Array { name :: String, type :: Type }
      , returnType :: Type
      , body :: AST
      }
  -- | Value type cast
  | Cast Type AST
  -- | Function application
  | App AST (Array AST)
  -- | Variable
  | Var String
  -- | Unique system-wide name/constant
  | Symbol String
  -- | A block of expressions in braces
  | Block (Array AST)
  -- | An #include
  | Include
      { path :: String
      }
  -- | A variable introduction and optional initialization
  | VariableIntroduction
      { name :: String
      , type :: Type
      , qualifiers :: Array ValueQualifier
      , initialization :: Maybe AST
      , managed :: Boolean
      }
  -- | A variable assignment
  | Assignment AST AST
  -- | While loop
  | While AST AST
  -- | If-then-else statement
  | IfElse AST AST (Maybe AST)
  -- | Return statement
  | Return AST
  -- | Continue statement
  | Continue
  -- | Empty statement/expression
  | NoOp
  -- | Marker for header/source split
  | EndOfHeader
  -- | Raw C
  | Raw String

  -- | Define a tag value for a purs_cons lookup
  -- | XXX a more general #define AST would be desirable later.
  | DefineTag String Int

  | Null

derive instance genericAST :: Rep.Generic AST _

instance eqAST :: Eq AST where
  eq x = genericEq x

instance showAST :: Show AST where
  show x = genericShow x

-- TODO: make this stack safe
everywhere :: (AST -> AST) -> AST -> AST
everywhere f = go
  where
  go (Block xs) =
    f $ Block $ go <$> xs
  go (Binary i a b) =
    f $ Binary i (go a) (go b)
  go (ArrayLiteral xs) =
    f $ ArrayLiteral $ go <$> xs
  go (Indexer a b) =
    f $ Indexer (go a) (go b)
  go (StructLiteral x) =
    f $
      StructLiteral $
        go <$> x
  go (ObjectLiteral xs) =
    f $
      ObjectLiteral $
        xs <#> \{ key, value } ->
          { key: go key
          , value: go value
          }
  go (Accessor a b) =
    f $ Accessor (go a) (go b)
  go (Function (x@{ body })) =
    f $ Function $ x { body = go <$> body }
  go (Lambda (x@{ body })) =
    f $ Lambda $ x { body = go body }
  go (Cast i b) =
    f $ Cast i (go b)
  go (App a xs) =
    f $ App (go a) (go <$> xs)
  go (VariableIntroduction x@{ initialization }) =
    f $
      VariableIntroduction $
        x { initialization = go <$> initialization
          }
  go (Assignment a b) =
    f $ Assignment (go a) (go b)
  go (While a b) =
    f $ While (go a) (go b)
  go (IfElse a b mC) =
    f $ IfElse (go a) (go b) (go <$> mC)
  go (Return a) =
    f $ Return (go a)
  go x =
    f x

-- TODO: make this stack safe
everything :: ∀ a. (a -> a -> a) -> (AST -> a) -> AST -> a
everything combine toA = go
  where
  go j@(Block xs) =
    A.foldl combine (toA j) $
      go <$> xs
  go j@(Binary _ a b) =
    toA j `combine` go a `combine` go b
  go j@(ArrayLiteral xs) =
    A.foldl combine (toA j) $
      go <$> xs
  go j@(Indexer a b) =
    toA j `combine` go a `combine` go b
  go j@(StructLiteral x) =
    A.foldl combine (toA j) $
      go <$> x
  go j@(ObjectLiteral xs) =
    A.foldl combine (toA j) $
      xs <#> \{ key, value } ->
        toA key `combine` toA value
  go j@(Accessor a b) =
    toA j `combine` go a `combine` go b
  go j@(Function (x@{ body: Nothing })) =
    toA j
  go j@(Function (x@{ body: Just body })) =
    toA j `combine` go body
  go j@(Lambda (x@{ body })) =
    toA j `combine` go body
  go j@(Cast _ b) =
    toA j `combine` go b
  go j@(App a xs) =
    A.foldl combine (toA j `combine` go a) $
      go <$> xs
  go j@(VariableIntroduction x@{ initialization: Nothing }) =
    toA j
  go j@(VariableIntroduction x@{ initialization: Just i }) =
    toA j `combine` go i
  go j@(Assignment a b) =
    toA j `combine` go a `combine` go b
  go j@(While a b) =
    toA j `combine` go a `combine` go b
  go j@(IfElse a b Nothing) =
    toA j `combine` go a `combine` go b
  go j@(IfElse a b (Just x)) =
    toA j `combine` go a `combine` go b `combine` go x
  go j@(Return a) =
    toA j `combine` go a
  go x =
    toA x
