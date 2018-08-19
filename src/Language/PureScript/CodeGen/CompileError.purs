module Language.PureScript.CodeGen.CompileError
  ( CompileError(..)
  ) where

data CompileError
  = InternalError String
  | NotImplementedError String
