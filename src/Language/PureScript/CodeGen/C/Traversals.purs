module Language.PureScript.CodeGen.C.Traversals where

import Prelude

import Control.Monad.Writer (execWriter, runWriterT, tell)
import Data.Array as A
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse_)
import Data.Tuple (fst, snd)
import Language.PureScript.CodeGen.C.AST (AST)
import Language.PureScript.CodeGen.C.AST as AST
