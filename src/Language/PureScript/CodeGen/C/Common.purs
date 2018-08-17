module Language.PureScript.CodeGen.C.Common
  ( safeName
  , dotsTo
  ) where

import Prelude
import Data.String.CodeUnits as Str

-- TODO
safeName :: String -> String
safeName name = name <> "$"

dotsTo :: Char -> String -> String
dotsTo chr' str =
  Str.fromCharArray $
    Str.toCharArray str <#> \c ->
      if c == '.'
        then chr'
        else c
