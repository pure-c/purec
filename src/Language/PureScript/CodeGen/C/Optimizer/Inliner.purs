module Language.PureScript.CodeGen.C.Optimizer.Inliner
  ( unThunk
  , inlineVariables
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Data.Array as A
import Data.Maybe (Maybe(..))
import Debug.Trace (traceM)
import Language.PureScript.CodeGen.C.AST (AST, everywhere, everywhereM)
import Language.PureScript.CodeGen.C.AST as AST
import Language.PureScript.CodeGen.C.Optimizer.Common (isReassigned, isRebound, isUpdated, replaceIdent, shouldInline)
import Language.PureScript.CodeGen.CompileError (CompileError)

unThunk :: AST -> AST
unThunk = everywhere convert
  where
  convert :: AST -> AST
  convert block@(AST.Block []) = block
  convert block@(AST.Block asts) =
    case A.unsnoc asts of
      Just
        { last:
             AST.Return
              (AST.App
                (AST.Function
                  { arguments: [], body: Just (AST.Block body) })
                [])
        , init
        } ->
        AST.Block $ init <> body
      _ ->
        block
  convert ast = ast

inlineVariables
  :: ∀ m
   . Monad m
  => MonadError CompileError m
  => AST
  -> m AST
inlineVariables = everywhereM $ mapBlock (go [])
  where
  mapBlock f (AST.Block sts) = AST.Block <$> f sts
  mapBlock _  x = pure x

  go acc =
    A.uncons >>> case _ of
      Nothing ->
        pure $ A.reverse acc
      Just
        ({ head:
            head@AST.VariableIntroduction
              { name
              , initialization: Just ast
              }
        , tail
        }) -> do
          canBeInlined <-
            A.foldM (\canInline x ->
              -- XXX: we could be lazier here
              (canInline && _) <$> ado
                isRebound' <- isRebound ast x
                isUpdated' <- isUpdated name x
                in
                  not $
                    isRebound' ||
                    isUpdated' ||
                    isReassigned name x
            ) (shouldInline ast) tail
          if canBeInlined
            then
              go acc $
                replaceIdent name ast <$> tail
            else
              go (head A.: acc) tail
      Just { head, tail } -> do
        go (head A.: acc) tail
