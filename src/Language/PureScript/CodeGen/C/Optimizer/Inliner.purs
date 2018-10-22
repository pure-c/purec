module Language.PureScript.CodeGen.C.Optimizer.Inliner
  ( unThunk
  , inlineVariables
  , inlineCommonValues
  , inlineFnComposition
  , etaConvert
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Data.Array as A
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple.Nested ((/\))
import Debug.Trace (traceM)
import Language.PureScript.CodeGen.C.AST (AST)
import Language.PureScript.CodeGen.C.AST as AST
import Language.PureScript.CodeGen.C.Common (anyM, freshName)
import Language.PureScript.CodeGen.C.Optimizer.Common (isDict, isReassigned, isRebound, isUpdated, replaceIdent, replaceIdents, shouldInline)
import Language.PureScript.CodeGen.CompileError (CompileError)
import Language.PureScript.CodeGen.Runtime as R
import Language.PureScript.CodeGen.SupplyT (class MonadSupply)
import Language.PureScript.Constants as C

etaConvert
  :: ∀ m
   . Functor m
  => Apply m
  => Monad m
  => MonadError CompileError m
  => AST
  -> m AST
etaConvert = AST.everywhereM go
  where
  go
    x@(AST.Block
      [ AST.Return
          (AST.App
            (AST.Function
              { arguments: args
              , body: Just block@(AST.Block body)
              }) passedArgs
          )])
    | A.all shouldInline passedArgs = ado
      ok <- ado
        y <- anyM (isRebound <@> block) $ map AST.Var (_.name <$> args)
        z <- anyM (isRebound <@> block) passedArgs
        in not y && not z
      in if ok
        then
          AST.Block $
            body <#> do
              replaceIdents $
                Map.fromFoldable $
                  A.zip (_.name <$> args) passedArgs
        else x
  go
    (AST.Function
      { arguments: []
      , body: Just
          (AST.Block
            [ AST.Return
                (AST.App fn [])
            ])
      }) = pure fn
  go x = pure x


unThunk :: AST -> AST
unThunk = AST.everywhere go
  where
  go :: AST -> AST
  go block@(AST.Block []) = block
  go block@(AST.Block asts) =
    case A.unsnoc asts of
      Just
        { init
        , last:
             AST.Return
              (AST.App (AST.Var "purs_any_app")
                [ AST.Function
                    { arguments: [], body: Just (AST.Block body) }
                , AST.Null
                ])
        } ->
        AST.Block $ init <> body
      _ ->
        block
  go ast = ast

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

-- (f <<< g $ x) = f (g x)
-- (f <<< g)     = \x -> f (g x)
inlineFnComposition
  :: ∀ m
   . Applicative m
  => MonadSupply m
  => Monad m
  => AST
  -> m AST
inlineFnComposition = AST.everywhereTopDownM go
  where
  go
    (AST.App (AST.Var "purs_any_app")
      [ AST.App (AST.Var "purs_any_app")
        [ AST.App (AST.Var "purs_any_app")
          [ AST.App  (AST.Var "purs_any_app")[ fn, dict], x], y], z])
    | isFnCompose dict fn =
        pure $
          AST.App R.purs_any_app
            [ x
            , AST.App R.purs_any_app [ y, z ]
            ]
    | isFnComposeFlipped dict fn =
        pure $
          AST.App R.purs_any_app
            [ y
            , AST.App R.purs_any_app [ x, z ]
            ]

  go
    (AST.App (AST.Var "purs_any_app")
     [ AST.App (AST.Var "purs_any_app")
       [ AST.App (AST.Var "purs_any_app") [ fn, dict ], x ], y ])
    | isFnCompose dict fn = ado
        arg <- freshName
        in
          AST.Function
            { name: Nothing
            , arguments: [ { name: arg, type: R.any } ]
            , returnType: R.any
            , qualifiers: []
            , variadic: false
            , body: Just $
                AST.Block
                  [ AST.Return $
                      AST.App R.purs_any_app
                        [ x
                        , AST.App R.purs_any_app
                            [ y
                            , AST.Var arg
                            ]
                        ]
                  ]
            }
    | isFnComposeFlipped dict fn = ado
        arg <- freshName
        in
          AST.Function
            { name: Nothing
            , arguments: [ { name: arg, type: R.any } ]
            , returnType: R.any
            , qualifiers: []
            , variadic: false
            , body: Just $
                AST.Block
                  [ AST.Return $
                      AST.App R.purs_any_app
                        [ y
                        , AST.App R.purs_any_app
                            [ x
                            , AST.Var arg
                            ]
                        ]
                  ]
            }
  go x = pure x

  isFnCompose dict fn =
    isDict (C.controlSemigroupoid /\ C.semigroupoidFn) dict &&
    isDict (C.controlSemigroupoid /\ C.compose) fn

  isFnComposeFlipped dict fn =
    isDict (C.controlSemigroupoid /\ C.semigroupoidFn) dict &&
    isDict (C.controlSemigroupoid /\ C.composeFlipped) fn
