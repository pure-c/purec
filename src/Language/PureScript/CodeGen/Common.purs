module Language.PureScript.CodeGen.Common
  ( runModuleName
  ) where

import Prelude

import CoreFn.Names as C
import Data.Array as A
import Data.Newtype (unwrap)

runModuleName :: C.ModuleName -> String
runModuleName (C.ModuleName pieces) =
  A.intercalate "." $ unwrap <$> pieces
