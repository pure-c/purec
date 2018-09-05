module Language.PureScript.CodeGen.C.AST
  ( UnaryOperator(..)
  , BinaryOperator(..)
  , AST(..)
  , Type(..)
  , PrimitiveType(..)
  , TypeQualifier(..)
  , ValueQualifier
  ) where

import Prelude

import CoreFn.Ann as C
import Data.Either (Either)
import Data.Generic.Rep as Rep
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
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

-- | Data type for simplified C++11 expressions
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
  -- | An C++ struct declaration (name, members)
  | Struct String (Array AST)
  -- | A C++ #include
  | Include
      { path :: String
      }
  -- | A variable introduction and optional initialization
  | VariableIntroduction
      { name :: String
      , type :: Type
      , qualifiers :: Array ValueQualifier
      , initialization :: Maybe AST
      }
  -- | A variable assignment
  | Assignment AST AST
  -- | While loop
  | While AST AST
  -- | If-then-else statement
  | IfElse AST AST (Maybe AST)
  -- | Switch statement
  | Switch AST (Array (AST /\ AST)) (Maybe AST)
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
