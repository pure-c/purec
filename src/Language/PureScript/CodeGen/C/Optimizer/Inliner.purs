module Language.PureScript.CodeGen.C.Optimizer.Inliner
  ( unThunk
  , inlineVariables
  , inlineCommonValues
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Data.Array as A
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Language.PureScript.CodeGen.C.AST (AST, everywhere)
import Language.PureScript.CodeGen.C.AST as AST
import Language.PureScript.CodeGen.C.Optimizer.Common (isDict, isReassigned, isRebound, isUpdated, replaceIdent, shouldInline)
import Language.PureScript.CodeGen.CompileError (CompileError)
import Language.PureScript.CodeGen.Runtime as R
import Language.PureScript.Constants as C

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
inlineVariables = AST.everywhereM
  case _ of
    AST.Block xs ->
      AST.Block <$> go [] xs
    x ->
      pure x
  where

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
              if not canInline
                then pure false
                else
                  ado
                  -- XXX: we could be lazier here
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

inlineCommonValues :: AST -> AST
inlineCommonValues = AST.everywhere go
  where

  -- inline integer negation
  go (AST.App
        (AST.Var "purs_any_app")
        [ AST.App (AST.Var "purs_any_app") [ fn, dict ], x ])
    | isDict (C.dataRing /\ C.ringInt) dict &&
      isDict (C.dataRing /\ C.negate) fn =
      AST.App R.purs_any_int_neg [ x ]

  go x = x
