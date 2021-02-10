-- | This module contains common functionality, much of which is focused on
-- | dealing with generated variable identifiers.
-- TODO: Avoid using '$' and generate standard compatible identifiers
module Language.PureScript.CodeGen.C.Common
  ( safeName
  , safeConstructorName
  , dotsTo
  , freshName
  , freshInternalName
  , freshInternalName'
  , isInternalVariable
  , prefixInternalVar
  , allM
  , allM'
  , anyM
  ) where

import Prelude

import Data.Either (fromRight)
import Data.Foldable (class Foldable, foldM)
import Data.Maybe (isJust)
import Data.Newtype (wrap)
import Data.String (stripPrefix) as Str
import Data.String.CodeUnits (fromCharArray, toCharArray) as Str
import Data.String.Regex (regex)
import Data.String.Regex (replace) as Regex
import Data.String.Regex.Flags (global) as Regex
import Language.PureScript.CodeGen.SupplyT (class MonadSupply, freshId)
import Partial.Unsafe (unsafePartial)

internalVarPrefix :: String
-- internalVarPrefix = "$_"
internalVarPrefix = "__purec__"  -- convient in gdb

prefixInternalVar :: String -> String
prefixInternalVar n = internalVarPrefix <> n

-- | Check if a given variable is internal.
isInternalVariable :: String -> Boolean
isInternalVariable = isJust <<< Str.stripPrefix (wrap internalVarPrefix)

-- | Derive a safe name.
-- TODO: Only append '_' if necessary
safeName :: String -> String
safeName name =
  let
    rex =
      unsafePartial $
        fromRight $
          regex "'" Regex.global
  in
    Regex.replace rex "$" $
      name <> "_$"

safeConstructorName :: String -> String
safeConstructorName n = internalVarPrefix <> "cons_" <> safeName n

dotsTo :: Char -> String -> String
dotsTo chr' str =
  Str.fromCharArray $
    Str.toCharArray str <#> \c ->
      if c == '.'
        then chr'
        else c

freshName
  :: ∀ m
   . Functor m
  => MonadSupply m
  => m String
freshName = ado
  id <- freshId
  in "$value" <> show id

freshInternalName'
  :: ∀ m
   . Functor m
  => MonadSupply m
  => String
  -> m String
freshInternalName' label = ado
  id <- freshId
  in internalVarPrefix <> label <> show id

freshInternalName
  :: ∀ m
   . Functor m
  => MonadSupply m
  => m String
freshInternalName = ado
  id <- freshId
  in internalVarPrefix <> "value" <> show id

allM :: ∀ f m a. Foldable f => Monad m => (a -> m Boolean) -> f a -> m Boolean
allM f =
  foldM
    (\a x ->
      if not a
         then pure false
         else f x
    ) true

allM' :: ∀ f m. Foldable f => Monad m => f (m Boolean) -> m Boolean
allM' = allM identity

anyM :: ∀ f m a. Foldable f => Monad m => (a -> m Boolean) -> f a -> m Boolean
anyM f =
  foldM
    (\a x ->
      if a
         then pure true
         else f x
    ) false
