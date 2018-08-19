module Language.PureScript.CodeGen.C.Common
  ( safeName
  , dotsTo
  ) where

import Prelude

import Data.String as Str
import Data.String.CodeUnits as Str

-- TODO: Only append '$' if necessary
safeName :: String -> String
safeName name =
  Str.replace (Str.Pattern "'") (Str.Replacement "$") $
    name <> "$"

dotsTo :: Char -> String -> String
dotsTo chr' str =
  Str.fromCharArray $
    Str.toCharArray str <#> \c ->
      if c == '.'
        then chr'
        else c
