module Language.PureScript.CodeGen.C.Common
  ( safeName
  , safeConstructorName
  , dotsTo
  , freshName
  , isInternalVariable
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

isInternalVariable :: String -> Boolean
isInternalVariable = isJust <<< Str.stripPrefix (wrap "$_")

-- TODO: Only append '$' if necessary
safeName :: String -> String
safeName name =
  let
    rex =
      unsafePartial $
        fromRight $
          regex "'" Regex.global
  in
    Regex.replace rex "$" $
      name <> "$"

safeConstructorName :: String -> String
safeConstructorName n = "$cons_" <> safeName n

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
