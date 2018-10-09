module Language.PureScript.CodeGen.C.Optimizer.Inliner
  ( unThunk
  , inlineVariables
  , inlineCommonValues
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Data.Array as A
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple.Nested ((/\))
import Language.PureScript.CodeGen.C.AST (AST, everywhere)
import Language.PureScript.CodeGen.C.AST as AST
import Language.PureScript.CodeGen.C.Optimizer.Common (isDict, isDict', isReassigned, isRebound, isUpdated, replaceIdent, shouldInline)
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

data StaticBinOps = Add | Sub | Mul

inlineCommonValues :: AST -> AST
inlineCommonValues = AST.everywhere go
  where

  -- int zero
  go (AST.App (AST.Var "purs_any_app") [ fn, dict ])
    | isDict (C.dataSemiring /\ C.semiringInt) dict &&
      isDict (C.dataSemiring /\ C.zero) fn =
        R.purs_any_int_zero

  -- number zero
  go (AST.App (AST.Var "purs_any_app") [ fn, dict ])
    | isDict (C.dataSemiring /\ C.semiringNumber) dict &&
      isDict (C.dataSemiring /\ C.zero) fn =
        R.purs_any_num_zero

  -- int one
  go (AST.App (AST.Var "purs_any_app") [ fn, dict ])
    | isDict (C.dataSemiring /\ C.semiringInt) dict &&
      isDict (C.dataSemiring /\ C.one) fn =
        R.purs_any_int_one

  -- number one
  go (AST.App (AST.Var "purs_any_app") [ fn, dict ])
    | isDict (C.dataSemiring /\ C.semiringNumber) dict &&
      isDict (C.dataSemiring /\ C.one) fn =
        R.purs_any_num_one

  -- boolean top
  go (AST.App (AST.Var "purs_any_app") [ fn, dict ])
    | isDict (C.dataBounded /\ C.boundedBoolean) dict &&
      isDict (C.dataBounded /\ C.top) fn =
        R.purs_any_true

  -- boolean bottom
  go (AST.App (AST.Var "purs_any_app") [ fn, dict ])
    | isDict (C.dataBounded /\ C.boundedBoolean) dict &&
      isDict (C.dataBounded /\ C.bottom) fn =
        R.purs_any_false

  -- unary integer operations
  go (AST.App (AST.Var "purs_any_app")
        [ AST.App (AST.Var "purs_any_app")
            [ fn, dict ], x ])
    | isDict (C.dataRing /\ C.ringInt) dict &&
      isDict (C.dataRing /\ C.negate) fn =
      case extractIntLit x of
        Just n ->
          AST.App R.purs_any_int_new [ AST.NumericLiteral (Left (-n)) ]
        Nothing ->
          AST.App R.purs_any_int_neg [ x ]

  -- binary integer operations
  go k@(AST.App (AST.Var "purs_any_app")
        [ AST.App (AST.Var "purs_any_app")
          [ AST.App (AST.Var "purs_any_app") [ fn, dict], x ], y ])
    | isDict (C.dataSemiring /\ C.semiringInt) dict &&
      isDict (C.dataSemiring /\ C.add) fn =
        intBinOp x y Add
    | isDict (C.dataSemiring /\ C.semiringInt) dict &&
      isDict (C.dataSemiring /\ C.mul) fn =
        intBinOp x y Mul
    | isDict (C.dataRing /\ C.ringInt) dict &&
      isDict (C.dataRing /\ C.sub) fn =
        intBinOp x y Sub

  go x = x

  -- inline operations on two integers. if two litera
  extractIntLit (AST.App fn [ AST.NumericLiteral (Left n) ])
    | fn == R.purs_any_int_new =
      Just n
  extractIntLit _ =
    Nothing

  toASTBinOp Add = AST.Add
  toASTBinOp Mul = AST.Multiply
  toASTBinOp Sub = AST.Subtract

  intBinOp x y op =
    case extractIntLit x, extractIntLit y of
      Just x', Just y' ->
        AST.App R.purs_any_int_new
          [ AST.NumericLiteral $ Left case op of
              Add -> x' + y'
              Sub -> x' - y'
              Mul -> x' * y'
          ]
      mLitX, mLitY ->
        AST.App R.purs_any_int_new
          [ AST.Binary (toASTBinOp op)
              (maybe
                (AST.App R.purs_any_get_int [ x ])
                (AST.NumericLiteral <<< Left)
                mLitX)
              (maybe
                (AST.App R.purs_any_get_int [ y ])
                (AST.NumericLiteral <<< Left)
                mLitY)
          ]
