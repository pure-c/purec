module Language.PureScript.CodeGen.C
  where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Reader (class MonadAsk, ReaderT(..), ask, runReaderT)
import Control.Monad.State (State, execState)
import Control.Monad.State as State
import Control.MonadPlus (guard)
import CoreFn.Ann as C
import CoreFn.Binders as C
import CoreFn.Expr as C
import CoreFn.Ident as C
import CoreFn.Literal as C
import CoreFn.Meta as C
import CoreFn.Module as C
import CoreFn.Names as C
import Data.Array (mapWithIndex)
import Data.Array as A
import Data.Either (Either(..), either)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.String.CodeUnits as Str
import Data.Traversable (foldr, for, for_, traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple.Nested ((/\), type (/\))
import Debug.Trace (spy, traceM)
import Effect.Ref (Ref)
import Foreign.Object as Object
import Language.PureScript.CodeGen.C.AST (AST)
import Language.PureScript.CodeGen.C.AST as AST
import Language.PureScript.CodeGen.C.AST as Type
import Language.PureScript.CodeGen.C.Common (safeName)
import Language.PureScript.CodeGen.C.File as F
import Language.PureScript.CodeGen.C.Pretty as P
import Language.PureScript.CodeGen.Common (runModuleName)
import Language.PureScript.CodeGen.CompileError (CompileError(..))
import Language.PureScript.CodeGen.Runtime as R
import Language.PureScript.CodeGen.Runtime as Runtime
import Language.PureScript.CodeGen.SupplyT (class MonadSupply, freshId)

type IsMain =
  Boolean

type Env =
  { module :: C.Module C.Ann
  }

freshName
  :: ∀ m
   . Functor m
  => MonadSupply m
  => m String
freshName = ado
  id <- freshId
  in "$value" <> show id

moduleToAST
  :: ∀ m
   . MonadError CompileError m
  => MonadSupply m
  => IsMain
  -> C.Module C.Ann
  -> m (Array AST)
moduleToAST isMain mod@(C.Module { moduleName, moduleImports, moduleExports, moduleDecls, moduleForeign }) =
  let
    cModuleName =
      runModuleName moduleName
    cModulePath =
      F.cModulePath moduleName
    cIncludes =
      ("runtime/purescript" A.: _) $
       map F.cModulePath $
        (A.catMaybes [
          let
            C.ModuleName pieces =
              moduleName
          in ado
            guard $ not (A.null moduleForeign)
            { init, last: C.ProperName last } <- A.unsnoc pieces
            in
              C.ModuleName $
                A.snoc init $
                  C.ProperName $ last <> "_ffi"
        ]) <> do
         A.filter
          (\(C.ModuleName pieces) ->
              case A.uncons pieces of
                Just { head: C.ProperName "Prim" } ->
                  false
                _ ->
                  true
          ) $
          A.difference <@> [ moduleName ] $
            _.moduleName <<< unwrap <$>
              moduleImports
  in runReaderT <@> { module: mod } $ do
    decls <-
      A.concat <$> do
        traverse (bindToAst true) moduleDecls

    let
      moduleHeader =
        F.withHeaderGuard moduleName $
          A.concat
            [ AST.Include <<< { path: _ } <$> cIncludes
            , [ P.empty ]
            , buildConstructorDeclsWithTags mod <#>
                \{ constructorName: C.ProperName constructorName, tag } ->
                  AST.DefineTag
                    (qualifiedVarName moduleName constructorName)
                    tag
            , F.toHeader decls
            ]

      moduleBody =
        A.concat
          [ [ AST.Include { path: cModulePath } ]
          , A.concat
              [ AST.Include <<< { path: _ } <$> cIncludes
              , [ P.empty ]
              , F.toBody decls
              ]
          , [ P.empty ]
          , if isMain
              then [ F.nativeMain, P.empty ]
              else []
          ]

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
  => Boolean -- ^ top-level?
  -> C.Bind C.Ann
  -> m (Array AST)
bindToAst isTopLevel (C.NonRec ann ident val) =
  A.singleton <$>
    declToAst isTopLevel (ann /\ ident) val
bindToAst isTopLevel (C.Rec vals) =
  for vals \((ann /\ ident) /\ val) ->
    declToAst isTopLevel (ann /\ ident) val

declToAst
  :: ∀ m
   . MonadError CompileError m
  => MonadSupply m
  => MonadAsk Env m
  => Boolean -- ^ is top level?
  -> (C.Ann /\ C.Ident)
  -> C.Expr C.Ann
  -> m AST
declToAst isTopLevel (x /\ ident) val = do
  { module: C.Module { moduleName } } <- ask
  name <-
    if isTopLevel
      then qualifiedVarName moduleName <$> identToVarName ident
      else identToVarName ident
  initAst <- exprToAst val
  pure $
    AST.VariableIntroduction
      { name
      , type: R.any'' [ Type.Const, Type.BlockStorage ]
      , qualifiers: []
      , initialization: Just initAst
      }
declToAst _ _ _ = throwError $ NotImplementedError "declToAst"

exprToAst
  :: ∀ m
   . MonadError CompileError m
  => MonadSupply m
  => MonadAsk Env m
  => C.Expr C.Ann
  -> m AST
exprToAst (C.Var ann ident) =
  case ann /\ ident of
    _ ->
      varToAst ident

  where
  varToAst :: C.Qualified C.Ident -> m AST
  varToAst (C.Qualified mModuleName ident) = do
    case mModuleName of
      Nothing ->
        AST.Var <$> identToVarName ident
      Just moduleName ->
        AST.Var <<< qualifiedVarName moduleName <$>
          identToVarName ident
exprToAst (C.Literal _ (C.NumericLiteral n)) =
  pure $
   AST.Cast (R.any) $
    AST.App
      (either (const R._PURS_ANY_INT) (const R._PURS_ANY_NUMBER) n)
      [ AST.NumericLiteral n
      ]
exprToAst (C.Literal _ (C.StringLiteral s)) =
  pure $
   AST.Cast (R.any) $
    AST.App
      R._PURS_ANY_STRING_FROM_LIT
      [ AST.StringLiteral s
      ]
exprToAst (C.Literal _ (C.CharLiteral c)) =
  pure $
   AST.Cast (R.any) $
    AST.App
      R._PURS_ANY_STRING_FROM_LIT
      [ AST.StringLiteral (Str.fromCharArray [ c ])
      ]
exprToAst (C.Literal _ (C.BooleanLiteral b)) =
  pure $
   AST.Cast (R.any) $
    AST.App
      R._PURS_ANY_INT
      [ AST.NumericLiteral $ Left $ if b then 1 else 0
      ]
exprToAst (C.Literal _ (C.ArrayLiteral xs)) = do
  asts <- traverse exprToAst xs
  pure $
   AST.Cast (R.any) $
    AST.App
      R._PURS_ANY_ARRAY $
      [ AST.App
        R.purs_vec_new_from_array $
        [ AST.NumericLiteral $ Left $ A.length xs
        ] <>
          if A.null asts
            then [ R._NULL ]
            else asts
      ]
exprToAst (C.Literal _ (C.ObjectLiteral kvps)) = do
  kvpAsts <-
    for kvps \(k /\ v) -> ado
      vAst <- exprToAst v
      in [ AST.StringLiteral k, vAst ]
  pure $
   AST.Cast (R.any) $
    if A.null kvps
      then
        R.purs_record_empty
      else
        AST.App
          R._PURS_ANY_RECORD $
            [ AST.App
              R.purs_record_new_from_kvps $
              [ AST.NumericLiteral $ Left $ A.length kvpAsts
              ] <> A.concat kvpAsts
            ]
exprToAst (C.Let _ binders val) = do
  bindersAsts <- A.concat <$> traverse (bindToAst false) binders
  valAst      <- exprToAst val
  pure $
    AST.App R.purs_any_app
      [
        AST.Lambda
          { arguments: []
          , returnType: R.any
          , body:
              AST.Block $
                bindersAsts <> [ AST.Return valAst ]
          }
      , AST.Null
      ]

exprToAst (C.Case (C.Ann { sourceSpan, type: typ }) exprs binders) = do
  assignments <- for exprs $
    exprToAst >=> \valueAst -> ado
      name <- freshName
      in
        { name
        , ast:
            AST.VariableIntroduction
              { name
              , type: R.any
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
                    (AST.App R.purs_any_eq_int
                      [ condAst, AST.NumericLiteral (Left 1)
                      ])
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
    AST.App R.purs_any_app
      [
        AST.Lambda
          { arguments: []
          , returnType: R.any
          , body:
              AST.Block $ A.concat
                [ map _.ast assignments
                , A.concat caseAlternativeAsts
                , [ R.assert' "Failed Pattern Match" ]
                ]
          }
      , AST.Null
      ]

  where
  binderToAst :: _ -> _ -> C.Binder C.Ann -> m (Array AST)
  binderToAst _ next (C.NullBinder _) = pure next
  binderToAst varName next (C.LiteralBinder _ lit) =
    case lit of
      C.NumericLiteral num ->
        pure
          [ AST.IfElse
              (AST.App
                (either (const R.purs_any_eq_int) (const R.purs_any_eq_float) num)
                [ AST.Var varName, AST.NumericLiteral num ])
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
              (AST.App
                R.purs_any_eq_int
                [ AST.Var varName, AST.NumericLiteral (Left 1) ])
              (AST.Block next)
              Nothing
          ]
      C.BooleanLiteral false ->
        pure
          [ AST.IfElse
              (AST.App
                R.purs_any_eq_int
                [ AST.Var varName, AST.NumericLiteral (Left 0) ])
              (AST.Block next)
              Nothing
          ]

      C.ArrayLiteral binders ->
        let
          go next' ix xs =
            case A.uncons xs of
              Nothing ->
                pure next'
              Just { head: x, tail: rest } -> do
                elementVar <- freshName
                next''     <- go next' (ix + 1) rest
                ast        <- binderToAst elementVar next'' x
                pure $
                  AST.VariableIntroduction
                    { name: elementVar
                    , type: R.any
                    , qualifiers: []
                    , initialization:
                        Just $
                          AST.Cast R.any $
                            AST.Indexer
                              (AST.NumericLiteral (Left ix))
                              (AST.Accessor
                                (AST.Raw "data")
                                (AST.App R.purs_any_get_array [ AST.Var varName ]))
                    } A.: ast
        in ado
          ast <- go next 0 binders
          in
            [ AST.IfElse
                (AST.Binary
                  AST.EqualTo
                  (AST.Accessor
                    (AST.Raw "length")
                    (AST.App R.purs_any_get_array [ AST.Var varName ]))
                  (AST.NumericLiteral $ Left (A.length binders)))
                (AST.Block ast)
                Nothing
            ]
      _ ->
        throwError $ NotImplementedError $ "binderToAst: literal: " <> show lit
  binderToAst varName next (C.VarBinder _ ident) = ado
    name <- identToVarName ident
    in
      AST.VariableIntroduction
        { name
        , type: R.any
        , qualifiers: []
        , initialization: Just $ AST.Var varName
        } A.: next


  binderToAst varName next
    (C.ConstructorBinder
      (C.Ann { meta: Just C.IsNewtype }) _ _ binders)
    | Just binder <- A.head binders
    = binderToAst varName next binder

  binderToAst varName next
    (C.ConstructorBinder
      (C.Ann
        { meta: Just (C.IsConstructor constructorType fields)
        }) _ (C.Qualified mConstructorModuleName (C.ProperName constructorName)) binders) =
    let
      go xs next' =
        case A.uncons xs of
          Nothing ->
            pure next'
          Just { head: ((index /\ field) /\ binder), tail: rest } -> do
            argVarName <- freshName
            next''     <- go rest next'
            ast        <- binderToAst argVarName next'' binder
            fieldName  <- identToVarName field
            pure $
              AST.VariableIntroduction
                { name: argVarName
                , type: R.any
                , qualifiers: []
                , initialization:
                    Just $
                      AST.Cast R.any $
                        AST.Indexer
                          (AST.NumericLiteral (Left index))
                          (AST.Accessor
                            (AST.Raw "values")
                            (AST.App R.purs_any_get_cons [ AST.Var varName ]))
                } A.: ast

    in
      case constructorType of
        C.ProductType ->
          go <@> next $ A.zip (A.mapWithIndex (/\) fields) binders
        C.SumType -> do
          tag <- ado
            moduleName <-
              case mConstructorModuleName of
                Just x ->
                  pure x
                Nothing -> ado
                  { module: C.Module { moduleName } } <- ask
                  in moduleName
            in qualifiedVarName moduleName constructorName
          asts <- go <@> next $ A.zip (A.mapWithIndex (/\) fields) binders
          pure
            [
              AST.IfElse
                (AST.Binary AST.EqualTo
                  (AST.App
                    R.purs_cons_get_tag
                    [ AST.App
                        R.purs_any_get_cons
                        [ AST.Var varName
                        ]
                    ]
                  )
                  (AST.Var tag)
                )
                (AST.Block asts)
                Nothing
            ]


  binderToAst _ _ x =
    throwError $ NotImplementedError $ "binderToAst " <> show x

exprToAst (C.Constructor _ typeName (C.ProperName constructorName) fields)
  | Just { init: initArgs, last: lastArg } <- A.unsnoc fields
  = do
  { module: C.Module { moduleName } } <- ask

  finalLambda <- do
    argName     <- identToVarName lastArg
    valuesName  <- freshName
    assignments <-
      traverseWithIndex <@> fields $ \i v -> ado
        name <- identToVarName v
        in
          AST.Assignment
            (AST.Indexer
              (AST.NumericLiteral (Left i))
              (AST.Var valuesName))
            (AST.Cast R.any' $ AST.Var name)
    pure $
     AST.Lambda
      { arguments: [ { name: argName, type: R.any } ]
      , returnType: R.any
      , body:
          AST.Block $
            [ AST.VariableIntroduction
                { name: valuesName
                , type: Type.Pointer R.any'
                , qualifiers: []
                , initialization:
                    Just $
                      AST.App
                        R._PURS_CONS_VALUES_NEW
                        [ AST.NumericLiteral (Left $ A.length fields) ]
                }
            ] <> assignments <> [
              AST.Return $
               AST.Cast R.any $
                AST.App R._PURS_ANY_CONS
                  [ AST.App R._PURS_CONS_LIT
                      [ let
                          tag =
                            qualifiedVarName moduleName constructorName
                        in AST.Var tag
                      , AST.Cast (Type.Pointer R.any) $
                          AST.Var valuesName
                      ]
                  ]
            ]
      }

  A.foldM
    (\body field -> ado
        argName' <- identToVarName field
        in
          AST.Lambda
            { arguments: [ { name: argName', type: R.any } ]
            , returnType: R.any
            , body: AST.Block [ AST.Return body ]
            }
    ) finalLambda $ A.reverse initArgs


-- note: fieldless constructors are emitted as top-level thunks that, once
-- evaluated, are stored in static storage.
exprToAst (C.Constructor _ typeName (C.ProperName constructorName) _) = do
  { module: C.Module { moduleName } } <- ask
  let
    constructorName' =
      qualifiedVarName moduleName constructorName
  pure $
    AST.App
      R._PURS_ANY_CONS
      [ AST.Cast (AST.RawType R.purs_cons_t []) $
          AST.StructLiteral $
            Object.empty
              # Object.insert "tag" (AST.Var constructorName')
              # Object.insert "values" AST.Null
      ]
exprToAst (C.App (C.Ann { type: typ }) ident expr) = do
  f   <- exprToAst ident
  arg <- exprToAst expr
  pure $ AST.App R.purs_any_app [f, arg]
exprToAst (C.Abs (C.Ann { type: typ }) indent expr) = do
  -- TODO: implement and apply `innerLambdas` to `bodyAst`
  bodyAst <- exprToAst expr
  argName <- identToVarName indent
  pure $
    AST.Lambda
      { arguments:
          [
            { name: argName
            , type: R.any
            }
          ]
      , returnType: R.any
      , body:
          AST.Block
            [ AST.Return bodyAst -- TODO: optIndexers/classes etc.
            ]
      }
exprToAst (C.Accessor _ k exp) = ado
  -- XXX: what if valueAst is not a record?
  valueAst <- exprToAst exp
  in AST.Accessor
    (AST.Raw "value")
    (AST.App
      R.purs_record_find_by_key
      [
        AST.App
          R.purs_any_get_record
          [ valueAst ]
      , AST.StringLiteral k
      ])

exprToAst e = throwError $ NotImplementedError $ "exprToAst " <> show e

qualifiedVarName :: C.ModuleName -> String -> String
qualifiedVarName (C.ModuleName pieces) varName =
  A.intercalate "_" (map unwrap pieces <> [ varName ])

identToVarName :: ∀ m. MonadError CompileError m => C.Ident -> m String
identToVarName (C.Ident name) = pure $ safeName name
identToVarName C.UnusedIdent = pure "$__unused"
identToVarName (C.GenIdent _ _) =
  throwError $
    InternalError "GenIdent in identToVarName"

type TypeName = C.ProperName
type ConstructorName = C.ProperName

buildConstructorDeclsWithTags
  :: ∀ a
   . C.Module a
  -> Array
      { typeName :: C.ProperName
      , constructorName :: C.ProperName
      , tag :: Int
      }
buildConstructorDeclsWithTags (C.Module { moduleDecls }) =
  let
    result =
      execState <@> Map.empty $
        for_ moduleDecls case _ of
          C.NonRec _ _ (C.Constructor _ typeName constructorName _) ->
            go typeName constructorName
          C.Rec xs ->
            for_ xs case _ of
              _ /\ (C.Constructor a typeName constructorName _) ->
                go typeName constructorName
              _ ->
                pure unit
          _ ->
            pure unit
  in
    A.concat $
      Map.toUnfoldable result <#> \(typeName /\ m) ->
        Map.toUnfoldable m <#> \(constructorName /\ tag) ->
          { typeName, constructorName, tag }

  where
  go
    :: C.ProperName -- ^ the type name
    -> C.ProperName -- ^ the constructor name
    -> State (Map TypeName (Map ConstructorName Int)) Unit
  go typeName constructorName = do
    m <- State.get
    case Map.lookup typeName m >>= Map.lookup constructorName of
      Just _ ->
        pure unit
      Nothing ->
        let
          m' = fromMaybe Map.empty (Map.lookup typeName m)
          ix = Map.size m'
        in
          State.put $
            Map.insert
              typeName
              (Map.insert constructorName ix m')
              m
