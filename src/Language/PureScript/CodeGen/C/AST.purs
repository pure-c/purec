module Language.PureScript.CodeGen.C.AST
  ( UnaryOperator(..)
  , BinaryOperator(..)
  , AST(..)
  , Type(..)
  , PrimitiveType(..)
  , TypeQualifier(..)
  , ValueQualifier(..)
  , FunctionQualifier(..)
  , everywhere
  , everywhereM
  , everything
  , everythingM
  , everywhereTopDown
  , everywhereTopDownM
  ) where

import Prelude

import Control.Monad.Trampoline (runTrampoline)
import Data.Array as A
import Data.Either (Either)
import Data.Generic.Rep as Rep
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Traversable (for, traverse)
import Data.Tuple.Nested ((/\))
import Foreign.Object (Object)
import Foreign.Object as Object
import Language.PureScript.CodeGen.C.Common (allM')

-- | Built-in unary operators
data UnaryOperator
  -- | Numeric negation
  = Negate
  -- | Boolean negation
  | Not

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

data FunctionQualifier
  = ModuleInternal

derive instance genericFunctionQual :: Rep.Generic FunctionQualifier _

instance eqFunctionQual :: Eq FunctionQualifier where
  eq = genericEq

instance showFunctionQual :: Show FunctionQualifier where
  show = genericShow

data ValueQualifier

derive instance genericValueQual :: Rep.Generic ValueQualifier _

instance eqValueQual :: Eq ValueQualifier where
  eq = genericEq

instance showValueQual :: Show ValueQualifier where
  show = genericShow

data TypeQualifier
  = Const

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
      { name :: Maybe String
      , arguments :: Array { name :: String, type :: Type }
      , returnType :: Type
      , qualifiers :: Array FunctionQualifier
      , variadic :: Boolean
      , body :: Maybe AST
      }

  -- | Value type cast
  | Cast Type AST

  -- | Function application
  | App AST (Array AST)

  -- | Variable
  | Var String

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

  -- | Marker for header/source split
  | EndOfHeader

  -- | Raw C
  | Raw String

  -- | Define a tag value for a purs_cons lookup
  -- | XXX a more general #define AST would be desirable later.
  | DefineTag String Int

  -- | NULL
  | Null

  -- | Statement expression
  | StatementExpression AST

instance eqAST :: Eq AST where
  eq u v = runTrampoline $ go u v
    where
    go (NumericLiteral a) (NumericLiteral a') = pure $ a == a'
    go (StringLiteral a) (StringLiteral a') = pure $ a == a'
    go (CharLiteral a) (CharLiteral a') = pure $ a == a'
    go (StructLiteral as) (StructLiteral as') =
      goArr'
        (\(key /\ value) (key' /\ value') ->
          allM'
            [ pure $ key == key'
            , go value value'
            ])
        (Object.toUnfoldable as) (Object.toUnfoldable as')
    go (Unary a b) (Unary a' b') =
      allM'
        [ pure $ a == a'
        , go b b'
        ]
    go (Binary a b c) (Binary a' b' c') =
      allM'
        [ pure $ a == a'
        , go b b'
        , go c c'
        ]
    go (ArrayLiteral as) (ArrayLiteral as') =
      goArr as as'
    go (Enum
          { name: a
          , members: b
          })
       (Enum
          { name: a'
          , members: b'
          }) = pure $ a == a' && b == b'
    go (Indexer a b) (Indexer a' b') =
      allM'
        [ go a a'
        , go b b'
        ]
    go (ObjectLiteral as) (ObjectLiteral as') =
      goArr'
        (\{ key, value } { key: key', value: value' } ->
          allM'
            [ go key key'
            , go value value'
            ])
        as as'
    go (Accessor a b) (Accessor a' b') =
      allM'
        [ go a a'
        , go b b'
        ]
    go (Function
          { name: a
          , arguments: bs
          , returnType: c
          , qualifiers: ds
          , variadic: e
          , body: mF
          })
       (Function
          { name: a'
          , arguments: bs'
          , returnType: c'
          , qualifiers: ds'
          , variadic: e'
          , body: mF'
          }) =
      allM'
        [ pure $ a == a' && bs == bs' && c == c' && ds == ds' && e == e'
        , goMay mF mF'
        ]
    go (Cast t a) (Cast t' a') =
      allM'
        [ pure $ t == t'
        , go a a'
        ]
    go (App a bs) (App a' bs') =
      allM'
        [ go a a'
        , goArr bs bs'
        ]
    go (Var a) (Var a') = pure $ a == a'
    go (Block as) (Block as') =
      goArr as as'
    go (Include { path: a }) (Include { path: a' }) = pure $ a == a'
    go (VariableIntroduction
          { name: a
          , type: b
          , qualifiers: cs
          , initialization: mD
          })
       (VariableIntroduction
          { name: a'
          , type: b'
          , qualifiers: cs'
          , initialization: mD'
          }) =
      allM'
        [ pure $ a == a' && b == b' && cs == cs'
        , goMay mD mD'
        ]
    go (Assignment a b) (Assignment a' b') =
      allM'
        [ go a a'
        , go b b'
        ]
    go (While a b) (While a' b') =
      allM'
        [ go a a'
        , go b b'
        ]
    go (IfElse a b mC) (IfElse a' b' mC') =
      allM'
        [ go a a'
        , go b b'
        , goMay mC mC'
        ]
    go (Return a) (Return a') = go a a'
    go Continue Continue = pure true
    go EndOfHeader EndOfHeader = pure true
    go (Raw a) (Raw a') = pure $ a == a'
    go (DefineTag a b) (DefineTag a' b') = pure $ a == a' && b == b'
    go Null Null = pure true
    go (StatementExpression x) (StatementExpression x') = go x x'
    go _ _ = pure false

    goMay mX mX' =
      case mX, mX' of
        Just x, Just x' ->
          go x x'
        Nothing, Nothing ->
          pure true
        _, _ ->
          pure false

    goArr' :: ∀ a. (a -> a -> _ Boolean) -> Array a -> Array a -> _
    goArr' f xs ys =
      allM' $
        [ pure $ A.length xs == A.length ys
        ] <> A.zipWith f xs ys

    goArr xs ys =
      goArr' go xs ys

derive instance genericAST :: Rep.Generic AST _

-- instance eqAST :: Eq AST where
--   eq x y = genericEq x y

instance showAST :: Show AST where
  show x = genericShow x

-- TODO: make this stack safe
everywhere :: (AST -> AST) -> AST -> AST
everywhere f ast =
  runTrampoline $
    everywhereM (pure <<< f) ast

everywhereM
  :: ∀ m
   . Monad m
  => (AST -> m AST)
  -> AST
  -> m AST
everywhereM f = go
  where
  go (Block xs) =
    f =<< do
      Block
        <$> traverse go xs
  go (Unary i a) =
    f =<< do
      Unary i
        <$> go a
  go (Binary i a b) =
    f =<< do
      Binary i
        <$> go a
        <*> go b
  go (ArrayLiteral xs) =
    f =<< do
      ArrayLiteral
        <$> traverse go xs
  go (Indexer a b) =
    f =<< do
      Indexer
        <$> go a
        <*> go b
  go (StructLiteral x) =
    f =<< do
      StructLiteral
        <$> traverse go x
  go (ObjectLiteral xs) =
    f =<< do
      ObjectLiteral <$>
        for xs \{ key, value } ->
          { key: _, value: _ }
            <$> go key
            <*> go value
  go (Accessor a b) =
    f =<< do
      Accessor
        <$> go a
        <*> go b
  go (Function (x@{ body })) =
    f =<< do
      Function <<< x { body = _ }
        <$> traverse go body
  go (Cast i b) =
    f =<< do
      Cast i <$> go b
  go (App a xs) =
    f =<< do
      App
        <$> go a
        <*> traverse go xs
  go (VariableIntroduction x@{ initialization }) =
    f =<< do
      VariableIntroduction <<<
        x { initialization = _
          } <$> traverse go initialization
  go (Assignment a b) =
    f =<< do
      Assignment
        <$> go a
        <*> go b
  go (While a b) =
    f =<< do
      While
        <$> go a
        <*> go b
  go (IfElse a b mC) =
    f =<< do
      IfElse
        <$> go a
        <*> go b
        <*> traverse go mC
  go (Return a) =
    f =<< do
      Return <$> go a
  go (StatementExpression a) =
    f =<< do
      StatementExpression <$> go a
  go x =
    f x

-- -- TODO: make this stack safe
everything :: ∀ a. (a -> a -> a) -> (AST -> a) -> AST -> a
everything f toA ast =
  runTrampoline $
    everythingM f (pure <<< toA) ast

everythingM
  :: ∀ a f
   . Applicative f
  => (a -> a -> a)
  -> (AST -> f a)
  -> AST
  -> f a
everythingM combine toA = go
  where
  go j@(Block xs) =
    A.foldl combine
      <$> toA j
      <*> traverse go xs
  go j@(Unary _ a) =
    combine
      <$> toA j
      <*> go a
  go j@(Binary _ a b) =
    combine
      <$> toA j
      <*> do
        combine
          <$> go a
          <*> go b
  go j@(ArrayLiteral xs) =
    A.foldl combine
      <$> toA j
      <*> traverse go xs
  go j@(Indexer a b) =
    combine
      <$> toA j
      <*> do
        combine
          <$> go a
          <*> go b
  go j@(StructLiteral x) =
    A.foldl combine
      <$> toA j
      <*> traverse go x
  go j@(ObjectLiteral xs) =
    A.foldl combine
      <$> toA j
      <*> do
        for xs\{ key, value } ->
          combine
            <$> toA key
            <*> toA value
  go j@(Accessor a b) =
    combine
      <$> toA j
      <*> do
        combine
          <$> go a
          <*> go b
  go j@(Function (x@{ body: Nothing })) =
    toA j
  go j@(Function (x@{ body: Just body })) =
    combine
      <$> toA j
      <*> go body
  go j@(Cast _ b) =
    combine
      <$> toA j
      <*> go b
  go j@(App a xs) = do
    A.foldl combine
      <$> (combine <$> toA j <*> go a)
      <*> traverse go xs
  go j@(VariableIntroduction x@{ initialization: Nothing }) =
    toA j
  go j@(VariableIntroduction x@{ initialization: Just i }) =
    combine
      <$> toA j
      <*> go i
  go j@(Assignment a b) =
    combine
      <$> toA j
      <*> do
        combine
          <$> go a
          <*> go b
  go j@(While a b) =
    combine
      <$> toA j
      <*> do
        combine
          <$> go a
          <*> go b
  go j@(IfElse a b Nothing) =
    combine
      <$> toA j
      <*> do
        combine
          <$> go a
          <*> go b
  go j@(IfElse a b (Just x)) =
    combine
      <$> toA j
      <*> do
        combine
          <$> go a
          <*> do
            combine
              <$> go b
              <*> go x
  go j@(Return a) =
    combine
      <$> toA j
      <*> go a
  go j@(StatementExpression a) =
    combine
      <$> toA j
      <*> go a
  go x =
    toA x

everywhereTopDown :: (AST -> AST) -> AST -> AST
everywhereTopDown f ast =
  runTrampoline $
    everywhereTopDownM (pure <<< f) ast

everywhereTopDownM
  :: ∀ m
   . Monad m
  => (AST -> m AST)
  -> AST
  -> m AST
everywhereTopDownM f = f'
  where
  f' x =
    f x >>= \y -> go y
  go (Block xs) =
    Block <$> traverse f' xs
  go (Unary i a) =
    Unary i <$> f' a
  go (Binary i a b) =
    Binary i <$> f' a <*> f' b
  go (ArrayLiteral xs) =
    ArrayLiteral <$> traverse f' xs
  go (Indexer a b) =
    Indexer <$> f' a <*> f' b
  go (StructLiteral x) =
    StructLiteral <$> traverse f' x
  go (ObjectLiteral xs) =
    ObjectLiteral <$>
      for xs \{ key, value } ->
        { key: _, value: _ }
          <$> f' key
          <*> f' value
  go (Accessor a b) =
    Accessor <$> f' a <*> f' b
  go (Function (x@{ body })) =
    Function <<< x { body = _ }
      <$> traverse f' body
  go (Cast i b) =
    Cast i <$> f' b
  go (App a xs) =
    App <$> f' a <*> traverse f' xs
  go (VariableIntroduction x@{ initialization }) =
    VariableIntroduction <<<
      x { initialization = _
        } <$> traverse f' initialization
  go (Assignment a b) =
    Assignment <$> f' a <*> f' b
  go (While a b) =
    While <$> f' a <*> f' b
  go (IfElse a b mC) =
    IfElse <$> f' a <*> f' b <*> traverse f' mC
  go (StatementExpression a) =
    StatementExpression <$> f' a
  go (Return a) =
    Return <$> f' a
  go x =
    f x
