module Language.PureScript.CodeGen.C.Common
  ( safeName
  , dotsTo
  , freshName
  ) where

import Prelude

import Data.Either (fromRight)
import Data.String.CodeUnits (fromCharArray, toCharArray) as Str
import Data.String.Regex (regex)
import Data.String.Regex (replace) as Regex
import Data.String.Regex.Flags (global) as Regex
import Language.PureScript.CodeGen.SupplyT (class MonadSupply, freshId)
import Partial.Unsafe (unsafePartial)

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
