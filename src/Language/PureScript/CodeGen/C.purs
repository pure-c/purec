module Language.PureScript.CodeGen.C
  where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Reader (class MonadAsk, ReaderT(..), ask, runReaderT)
import CoreFn.Ann as C
import CoreFn.Binders as C
import CoreFn.Expr as C
import CoreFn.Ident as C
import CoreFn.Literal as C
import CoreFn.Module as C
import CoreFn.Names as C
import Data.Array as A
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Traversable (for, traverse)
import Data.Tuple.Nested ((/\), type (/\))
import Debug.Trace (spy, traceM)
import Effect.Ref (Ref)
import Language.PureScript.CodeGen.C.AST (AST)
import Language.PureScript.CodeGen.C.AST as AST
import Language.PureScript.CodeGen.C.AST as Type
import Language.PureScript.CodeGen.C.Common (safeName)
import Language.PureScript.CodeGen.C.File as F
import Language.PureScript.CodeGen.C.Pretty as P
import Language.PureScript.CodeGen.C.Traversals (everythingOnAST)
import Language.PureScript.CodeGen.Common (runModuleName)
import Language.PureScript.CodeGen.Runtime as Runtime
import Language.PureScript.CodeGen.SupplyT (class MonadSupply, freshId)

type Env =
  { moduleName :: C.ModuleName
  }

freshName
  :: ∀ m
   . Functor m
  => MonadSupply m
  => m String
freshName = ado
  id <- freshId
  in "value" <> show id

data CompileError
  = InternalError String
  | NotImplementedError String

moduleToAST
  :: ∀ m
   . MonadError CompileError m
  => MonadSupply m
  => C.Module C.Ann
  -> m (Array AST)
moduleToAST mod@(C.Module { moduleName, moduleImports, moduleExports, moduleDecls }) =
  let
    cModuleName =
      runModuleName moduleName
    cIncludes =
      ("runtime/purescript" A.: _) $
       map F.cModuleName $
        A.delete (C.ModuleName [ C.ProperName "Prim" ]) $
          A.difference <@> [ moduleName ] $
            _.moduleName <<< unwrap <$>
              moduleImports
  in runReaderT <@> { moduleName } $ do
    decls <-
      map filterInlineFuncs <<< A.concat <$> do
        traverse bindToAst moduleDecls

    let
      moduleHeader =
        F.withHeaderGuard moduleName $
          A.concat
            [ AST.Include <<< { path: _ } <$> cIncludes
            , [ P.empty ]
            , F.toHeader decls
            ]

      moduleBody =
        A.concat
          [ [ AST.Include { path: cModuleName } ]
          , A.concat
              [ AST.Include <<< { path: _ } <$> cIncludes
              , [ P.empty ]
              , F.toBody decls
              ]
          , [ P.empty ]
          , if true -- F.isMain moduleName (TODO)
              then [ F.nativeMain, P.empty ]
              else []
          ]

    -- traceM moduleDecls
    -- traceM decls
    -- traceM moduleBody

    pure $
      A.concat $
        [ moduleHeader
        , [ AST.EndOfHeader ] -- TODO: return tuple instead
        , moduleBody
        ]

-- TODO: Implement
bindToAst
  :: ∀ m
   . MonadError CompileError m
  => MonadSupply m
  => MonadAsk Env m
  => C.Bind C.Ann
  -> m (Array AST)
bindToAst (C.NonRec ann ident val) =
  A.singleton <$>
    declToAst (ann /\ ident) val
bindToAst _ =
  throwError $ NotImplementedError "bindToAst"

declToAst
  :: ∀ m
   . MonadError CompileError m
  => MonadSupply m
  => MonadAsk Env m
  => (C.Ann /\ C.Ident)
  -> C.Expr C.Ann
  -> m AST
declToAst (_ /\ ident) val = do
  { moduleName } <- ask
  name <- identToVarName' moduleName ident
  initAst <- exprToAst val
  pure $
    AST.VariableIntroduction
      { name
      , type: Type.Pointer (Type.Any [])
      , qualifiers: []
      , initialization: Just initAst
      }
declToAst _ _ = throwError $ NotImplementedError "declToAst"

exprToAst
  :: ∀ m
   . MonadError CompileError m
  => MonadSupply m
  => MonadAsk Env m
  => C.Expr C.Ann
  -> m AST
exprToAst (C.Var ann ident) =
  case ann /\ ident of
    -- TODO: implement/port other cases:
    ---- exprToAst (Var (_, _, _, Just IsNewtype) ident) = return $ varToC ident
    ---- exprToAst (Var (_, _, _, Just (IsConstructor _ [])) ident) = return $ CApp (varToC ident) []
    ---- exprToAst (Var (_, _, Just ty, _) ident@(Qualified (Just _) _))
    ----   | arity ty > 0 = return . curriedVar $ varToC ident
    ---- exprToAst (Var (_, _, Nothing, _) ident@(Qualified (Just _) _))
    ----   | Just (ty, _, _) <- M.lookup ident (E.names env)
    ----   , arity ty > 0 = return . curriedVar $ varToC ident
    _ ->
      varToAst ident

  where
  varToAst :: C.Qualified C.Ident -> m AST
  varToAst (C.Qualified mModuleName ident) = do
    case mModuleName of
      Nothing ->
        AST.Var <$> identToVarName ident
      Just moduleName ->
        AST.Var <$> identToVarName' moduleName ident
exprToAst (C.Literal _ (C.NumericLiteral n)) = pure $ AST.NumericLiteral n
exprToAst (C.Literal _ (C.StringLiteral s)) = pure $ AST.StringLiteral s
exprToAst (C.Literal _ (C.CharLiteral c)) = pure $ AST.CharLiteral c
exprToAst (C.Literal _ (C.BooleanLiteral b)) = pure $ AST.BooleanLiteral b
exprToAst (C.Literal _ (C.ArrayLiteral xs)) = AST.ArrayLiteral <$> traverse exprToAst xs
-- exprToAst (C.Literal _ (C.ObjectLiteral ps)) =
--   AST.RecordLiteral <$>
--     for ps \(key /\ value) -> ado
--       value' <- exprToAst value
--       in { key: AST.StringLiteral key, value: value' }
exprToAst (C.Case (C.Ann { sourceSpan, type: typ }) exprs binders) = do
  assignments <- for exprs $
    exprToAst >=> \valueAst -> ado
      name <- freshName
      in
        { name
        , ast:
            AST.VariableIntroduction
              { name
              , type: Type.Pointer (Type.Any [])
              , qualifiers: []
              , initialization: Just valueAst
              }
        }

  caseAlternativeAsts :: Array (Array AST) <-
    for binders \(C.CaseAlternative { caseAlternativeBinders, caseAlternativeResult }) ->
      let
        go _ done [] =
          pure done
        go varNames done binders
          | Just { head: v, tail: vs } <- A.uncons varNames
          , Just { head: b, tail: bs } <- A.uncons binders
          = do
            done' <- go vs done bs
            binderToAst v done' b
        go _ _ _ =
          throwError $ InternalError "Invalid case alternative"
      in do
        altAsts <-
          case caseAlternativeResult of
            Left guards ->
              for guards \(guard /\ expr) -> ado
                condAst <- exprToAst guard
                valAst  <- exprToAst expr
                in
                  AST.IfElse
                    condAst
                    (AST.Block [ AST.Return valAst ])
                    Nothing
            Right expr ->
              A.singleton <<<
                AST.Return <$>
                  exprToAst expr

        go
          (map _.name assignments)
          altAsts
          caseAlternativeBinders

  pure $
    AST.App <@> [] $
      AST.Lambda
        { arguments: []
        , returnType: Type.Pointer (Type.Any [])
        , body:
            AST.Block $ A.concat
              [ map _.ast assignments
              , A.concat caseAlternativeAsts
              -- TODO: pattern error
              -- , [ failedPatternError valNames ]
              ]
        }

  where
  binderToAst :: _ -> _ -> C.Binder C.Ann -> m (Array AST)
  binderToAst _ next (C.NullBinder _) = pure next
  binderToAst varName next (C.LiteralBinder _ lit) =
    case lit of
      C.NumericLiteral num ->
        pure
          [ AST.IfElse
              (AST.Binary AST.EqualTo (AST.Var varName) (AST.NumericLiteral num))
              (AST.Block next)
              Nothing
          ]
      C.CharLiteral c ->
        pure
          [ AST.IfElse
              (AST.Binary AST.EqualTo (AST.Var varName) (AST.CharLiteral c))
              (AST.Block next)
              Nothing
          ]
      C.StringLiteral str ->
        pure
          [ AST.IfElse
              (AST.Binary AST.EqualTo (AST.Var varName) (AST.StringLiteral str))
              (AST.Block next)
              Nothing
          ]
      C.BooleanLiteral true ->
        pure
          [ AST.IfElse
              (AST.Var varName)
              (AST.Block next)
              Nothing
          ]
      C.BooleanLiteral false ->
        pure
          [ AST.IfElse
              (AST.Unary AST.Not (AST.Var varName))
              (AST.Block next)
              Nothing
          ]
      _ ->
        throwError $ NotImplementedError $ "binderToAst: literal: " <> show lit
  binderToAst varName next (C.VarBinder _ ident) = ado
    name <- identToVarName ident
    in
      AST.VariableIntroduction
        { name
        , type: Type.Pointer (Type.Any [])
        , qualifiers: []
        , initialization: Just $ AST.Var varName
        } A.: next
  binderToAst _ _ x =
    throwError $ NotImplementedError $ "binderToAst " <> show x
exprToAst (C.Constructor _ _ _ _) =
  pure AST.NoOp
exprToAst (C.App (C.Ann { type: typ }) ident expr) = do
  f   <- exprToAst ident
  arg <- exprToAst expr
  pure $ AST.App Runtime.purs_any_app [f, arg]
exprToAst (C.Abs (C.Ann { type: typ }) indent expr) = do
  -- TODO: implement and apply `innerLambdas` to `bodyAst`
  bodyAst <- exprToAst expr
  argName <- identToVarName indent
  pure $
    AST.Lambda
      { arguments:
          [
            { name: argName
            , type: Type.Pointer (Type.Any [])
            }
          ]
      , returnType: Type.Pointer (Type.Any [])
      , body:
          AST.Block
            [ AST.Return bodyAst -- TODO: optIndexers/classes etc.
            ]
      }
exprToAst e = throwError $ NotImplementedError $ "exprToAst " <> show e

identToVarName' :: ∀ m. MonadError CompileError m => C.ModuleName -> C.Ident -> m String
identToVarName' (C.ModuleName pieces) ident = ado
  name <- identToVarName ident
  in A.intercalate "_" (map unwrap pieces <> [ name ])

identToVarName :: ∀ m. MonadError CompileError m => C.Ident -> m String
identToVarName (C.Ident name) = pure $ safeName name
identToVarName C.UnusedIdent = pure "$__unused"
identToVarName (C.GenIdent _ _) =
  throwError $
    InternalError "GenIdent in identToVarName"

-- TODO: Implement
filterInlineFuncs :: AST -> AST
filterInlineFuncs = identity

allSymbols :: Array AST -> Array String
allSymbols asts =
  A.nub $
    A.concatMap (everythingOnAST symbols) asts
  where
  symbols :: AST -> Array String
  symbols (AST.Symbol s) = [s]
  symbols _ = []
