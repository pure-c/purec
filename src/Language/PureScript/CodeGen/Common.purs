module Language.PureScript.CodeGen.Common
  ( runModuleName
  ) where

import Prelude

import CoreFn.Names as C
import Data.Array as A
import Data.Newtype (unwrap)
import Language.PureScript.CodeGen.C.File as C

runModuleName :: C.ModuleName -> String
runModuleName = C.cModuleName
