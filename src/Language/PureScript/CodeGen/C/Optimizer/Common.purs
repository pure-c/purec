module Language.PureScript.CodeGen.C.Optimizer.Common
  ( mapBlock
  , isReassigned
  , isRebound
  , isUpdated
  , shouldInline
  , replaceIdent
  , replaceIdents
  , isDict
  , isDict'
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Data.Array as A
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Tuple.Nested ((/\), type (/\))
import Language.PureScript.CodeGen.C.AST (AST, everything, everythingM, everywhere)
import Language.PureScript.CodeGen.C.AST as AST
import Language.PureScript.CodeGen.CompileError (CompileError(..))

shouldInline :: AST -> Boolean
shouldInline (AST.Var _) = true
shouldInline (AST.NumericLiteral _) = true
shouldInline (AST.CharLiteral _) = true
shouldInline (AST.StringLiteral _) = true
shouldInline _ = false

mapBlock :: (Array AST -> Array AST) -> AST -> AST
mapBlock go (AST.Block sts) = AST.Block (go sts)
mapBlock _  x = x

replaceIdent :: String -> AST -> AST -> AST
replaceIdent var1 ast = everywhere replace
  where
  replace (AST.Var var2) | var1 == var2 = ast
  replace x = x

replaceIdents :: Map String AST -> AST -> AST
replaceIdents vars = everywhere replace
  where
  replace v@(AST.Var var) = fromMaybe v $ Map.lookup var vars
  replace x = x

isReassigned :: String -> AST -> Boolean
isReassigned var1 = everything (||) go
  where
  go (AST.Function { arguments }) = var1 `A.elem` (map _.name arguments)
  go (AST.VariableIntroduction { name }) = var1 == name
  go (AST.Assignment (AST.Var name) _) = var1 == name
  go _ = false

isRebound
  :: ∀ m
   . Monad m
  => MonadError CompileError m
  => AST -- ^ the target ast
  -> AST -- ^ the scope to inspect
  -> m Boolean
isRebound targetAst scopeAst = do
  A.foldM (\acc x ->
    disj acc
      <$> do
        disj (isReassigned x scopeAst)
          <$> isUpdated x scopeAst
    ) false $ everything (<>) variablesOf targetAst
  where
  variablesOf (AST.Var var) = [var]
  variablesOf _ = []

isUpdated
  :: ∀ m
   . Monad m
  => MonadError CompileError m
  => String
  -> AST
  -> m Boolean
isUpdated var1 = everythingM (||) go
  where
  go (AST.Assignment target _) =
    eq var1 <$>
      targetVariable target
  go _ =
    pure false

  targetVariable (AST.Var var) = pure var
  targetVariable (AST.Indexer _ tgt) = targetVariable tgt
  targetVariable (AST.Accessor _ tgt) = targetVariable tgt
  targetVariable _ =
    throwError $
      InternalError "Invalid argument to targetVariable"

-- | check if the given AST is performing a (specific) dictionary lookup
-- note: refer to `exprToAst` in 'Language.PureScript.CodeGen.C'
isDict :: (String /\ String) -> AST -> Boolean
isDict (moduleName /\ dictName) (AST.Var n) =
  n == moduleName <> "_" <> dictName <> "$" -- XXX document '$' suffix
isDict _ _ = false

isDict' :: Array (String /\ String) -> AST -> Boolean
isDict' xs ast = A.any (_ `isDict` ast) xs
