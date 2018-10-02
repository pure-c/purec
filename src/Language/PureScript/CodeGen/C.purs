module Language.PureScript.CodeGen.C
  where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Reader (class MonadAsk, ask, runReaderT, withReaderT)
import Control.Monad.State (State, execState)
import Control.Monad.State as State
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (runWriterT, tell)
import Control.MonadPlus (guard)
import CoreFn.Ann (Ann(..)) as C
import CoreFn.Binders (Binder(..)) as C
import CoreFn.Expr (Bind(..), CaseAlternative(..), Expr(..)) as C
import CoreFn.Ident (Ident(..)) as C
import CoreFn.Literal (Literal(..)) as C
import CoreFn.Meta (ConstructorType(..), Meta(..)) as C
import CoreFn.Module (Module(..)) as C
import CoreFn.Names (ModuleName(..), ProperName(..), Qualified(..)) as C
import Data.Array as A
import Data.Bifunctor (bimap)
import Data.Char as Int
import Data.Either (Either(..), either)
import Data.Function (on)
import Data.Identity (Identity(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (for, for_, traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\), type (/\))
import Debug.Trace (trace, traceM)
import Foreign.Object as Object
import Language.PureScript.CodeGen.C.AST (AST, everywhereTopDownM)
import Language.PureScript.CodeGen.C.AST as AST
import Language.PureScript.CodeGen.C.AST as Type
import Language.PureScript.CodeGen.C.Common (safeName)
import Language.PureScript.CodeGen.C.File as F
import Language.PureScript.CodeGen.C.Optimizer (optimize)
import Language.PureScript.CodeGen.C.Pretty as P
import Language.PureScript.CodeGen.Common (runModuleName)
import Language.PureScript.CodeGen.CompileError (CompileError(..))
import Language.PureScript.CodeGen.Runtime as AST
import Language.PureScript.CodeGen.Runtime as R
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
      ("purescript" A.: _) $
       map F.cModulePath $
        (A.catMaybes [
          ado
            guard $ not (A.null moduleForeign)
            in
              C.ModuleName
                [ C.ProperName $
                    F.dottedModuleName moduleName <> "_ffi"
                ]
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
    decls <- do
      decls <- A.concat <$> traverse (bindToAst true) moduleDecls
      eraseLambdas cModuleName =<< do
        hoistVarDecls <$>
          traverse optimize decls

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
              then
                [ F.nativeMain $
                    AST.Var $
                      safeName $
                        qualifiedVarName moduleName "main"
                , P.empty
                ]
              else []
          ]

    pure $
      A.concat $
        [ moduleHeader
        , [ AST.EndOfHeader ] -- TODO: return tuple instead
        , moduleBody
        ]

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
      , type: R.any'' [ Type.Const ]
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
   AST.Cast R.any $
    AST.App
      (either (const R.purs_any_int_new) (const R.purs_any_num_new) n)
      [ AST.NumericLiteral n
      ]
exprToAst (C.Literal _ (C.StringLiteral s)) =
  pure $
   AST.Cast R.any $
    AST.App
      R.purs_any_string_new
      [ AST.StringLiteral s
      ]
exprToAst (C.Literal _ (C.CharLiteral c)) =
  pure $
   AST.Cast R.any $
    AST.App
      R.purs_any_char_new
      [ AST.NumericLiteral $ Left $ Int.toCharCode c
      ]
exprToAst (C.Literal _ (C.BooleanLiteral b)) =
  pure $
    if b
      then R.purs_any_true
      else R.purs_any_false
exprToAst (C.Literal _ (C.ArrayLiteral xs)) = ado
  asts <- traverse exprToAst xs
  in
   AST.Cast (R.any) $
    AST.App
      R.purs_any_array_new $
      [ AST.App
        R.purs_vec_new_from_array $
        [ AST.NumericLiteral $ Left $ A.length xs
        ] <>
          if A.null asts
            then [ R._NULL ]
            else asts
      ]
exprToAst (C.Literal _ (C.ObjectLiteral kvps)) = ado
  kvpAsts <-
    for kvps \(k /\ v) -> ado
      vAst <- exprToAst v
      in [ AST.StringLiteral k, vAst ]
  in
   AST.Cast (R.any) $
    if A.null kvps
      then
        R.purs_record_empty
      else
        AST.App
          R.purs_any_record_new $
            [ AST.App
              R.purs_record_new_from_kvps $
              [ AST.NumericLiteral $ Left $ A.length kvpAsts
              ] <> A.concat kvpAsts
            ]
exprToAst (C.Let _ binders val) = ado
  bindersAsts <- A.concat <$> traverse (bindToAst false) binders
  valAst      <- exprToAst val
  in
    AST.App R.purs_any_app
      [
        AST.Function
          { name: Nothing
          , variadic: false
          , qualifiers: []
          , arguments: []
          , returnType: R.any
          , body:
              Just $
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
                      [ condAst
                      , AST.NumericLiteral (Left 1)
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
        AST.Function
          { name: Nothing
          , qualifiers: []
          , variadic: false
          , arguments: []
          , returnType: R.any
          , body:
              Just $
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
                (either (const R.purs_any_eq_int) (const R.purs_any_eq_num) num)
                [ AST.Var varName, AST.NumericLiteral num ])
              (AST.Block next)
              Nothing
          ]
      C.CharLiteral c ->
        pure
          [ AST.IfElse
              (AST.App
                R.purs_any_eq_char
                [ AST.Var varName, AST.CharLiteral c ])
              (AST.Block next)
              Nothing
          ]
      C.StringLiteral str ->
        pure
          [ AST.IfElse
              (AST.App
                R.purs_any_eq_string
                [ AST.Var varName, AST.StringLiteral str ])
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

      C.ObjectLiteral binders ->
        let
          go next' xs =
            case A.uncons xs of
              Nothing ->
                pure next'
              Just { head: (prop /\ binder), tail: rest } -> do
                propVar  <- freshName
                next''   <- go next' rest
                ast      <- binderToAst propVar next'' binder
                pure $
                  AST.VariableIntroduction
                    { name: propVar
                    , type: R.any
                    , qualifiers: []
                    , initialization:
                        Just $
                          AST.Accessor
                            (AST.Raw "value")
                            (AST.App
                              R.purs_record_find_by_key
                              [
                                AST.App
                                  R.purs_any_get_record
                                  [ AST.Var varName ]
                              , AST.StringLiteral prop
                              ])
                    } A.: ast
        in
          go next binders

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

  binderToAst _ _ (C.ConstructorBinder _ _ _ _) =
    throwError $ InternalError $
      "binderToAst: Invalid ConstructorBinder"

  binderToAst varName next (C.NamedBinder _ ident binder) = ado
    name <- identToVarName ident
    ast <- binderToAst varName next binder
    in
      AST.VariableIntroduction
        { name
        , type: R.any
        , qualifiers: []
        , initialization: Just $ AST.Var varName
        } A.: ast

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
          AST.Assignment false
            (AST.Indexer
              (AST.NumericLiteral (Left i))
              (AST.Var valuesName))
            (AST.Cast R.any $ AST.Var name)
    pure $
     AST.Function
      { name: Nothing
      , qualifiers: []
      , variadic: false
      , arguments: [ { name: argName, type: R.any } ]
      , returnType: R.any
      , body: Just $
          AST.Block $
            [ AST.VariableIntroduction
                { name: valuesName
                , type: Type.Pointer R.any
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
                AST.App R.purs_any_cons_new
                  [ AST.Var $ qualifiedVarName moduleName constructorName
                  , AST.Cast (Type.Pointer R.any) $ AST.Var valuesName
                  ]
            ]
      }

  A.foldM
    (\body field -> ado
        argName' <- identToVarName field
        in
          AST.Function
            { name: Nothing
            , qualifiers: []
            , variadic: false
            , arguments: [ { name: argName', type: R.any } ]
            , returnType: R.any
            , body: Just $ AST.Block [ AST.Return body ]
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
      R.purs_any_cons_new
      [ AST.Var constructorName'
      , AST.Null
      ]
exprToAst (C.App (C.Ann { type: typ }) ident expr) = do
  f   <- exprToAst ident
  arg <- exprToAst expr
  pure $ AST.App R.purs_any_app [f, arg]
exprToAst (C.Abs (C.Ann { type: typ }) indent expr) = do
  bodyAst <- exprToAst expr
  argName <- identToVarName indent
  pure $
    AST.Function
      { name: Nothing
      , qualifiers: []
      , variadic: false
      , arguments:
          [{ name: argName, type: R.any }]
      , returnType: R.any
      , body:
          Just $
            AST.Block
              [ AST.Return bodyAst
              ]
      }
exprToAst (C.Accessor _ k exp) = ado
  -- XXX: what if valueAst is not a record?
  valueAst <- exprToAst exp
  in
    AST.Accessor
      (AST.Raw "value")
      (AST.App
        R.purs_record_find_by_key
        [
          AST.App
            R.purs_any_get_record
            [ valueAst ]
        , AST.StringLiteral k
        ])
exprToAst (C.ObjectUpdate _ o ps) =
  -- TODO: Implement this
  throwError $ NotImplementedError $ "exprToAst: C.ObjectUpdate"

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

runIdentity :: ∀ a. Identity a -> a
runIdentity = unwrap

-- | Erase lambdas from the AST by creating tailor-made scope structures for
-- | every lambda we encounter.
-- |
-- | For example, given the following function:
-- |    foo = \a b -> b
-- | This is trivially representable as a couple of continuation functions:
-- |   struct foo_1_scope { const ANY * a; };
-- |   struct foo_2_scope { const ANY * a; const ANY * b; };
-- |   const ANY * foo_2 (const void * super, const ANY * b);
-- |   const ANY * foo_1 (const void * super, const ANY * a);
-- |
-- | XXX: we might have to run this pass *after* optimization passes ran in
-- |      order to not capture inlined and unused variables.
eraseLambdas
  :: ∀ m
   . Monad m
  => MonadSupply m
  => String -- ^ lambda prefix
  -> Array AST
  -> m (Array AST)
eraseLambdas moduleName asts =
  ado
    asts' /\ toplevels <-
      runWriterT $
        runReaderT (traverse go asts) $
          { isTopLevel: true
          , function: Nothing
          , lhs: Nothing
          , bindings: Set.empty
          }
    in toplevels <> asts'

  where
  go =
    case _ of
      AST.Assignment true (AST.Var v) b ->
        AST.Assignment true (AST.Var v) <$> do
          withReaderT (_ { lhs = Just v }) ado
            b' <- go b
            in
              AST.StatementExpression $
                AST.Block
                  -- TODO: only do this dance if self-recursive binding
                  [ AST.VariableIntroduction
                      { name: "$ivalue"
                      , type: Type.Pointer R.any
                      , qualifiers: []
                      , initialization:
                          Just $
                            AST.App R.purs_indirect_value_new []
                      }
                  , AST.VariableIntroduction
                      { name: "$value"
                      , type: R.any
                      , qualifiers: []
                      , initialization: Just $ b'
                      }
                  , AST.App R.purs_indirect_value_assign
                      [ AST.Var "$ivalue"
                      , AST.Var "$value"
                      ]
                  , AST.Var "$value"
                  ]
      AST.Function x@{ name, arguments, body: Just body } ->
        withReaderT (_ { function = name, isTopLevel = false }) do
          eraseLambda { arguments, body }
      ast@AST.VariableIntroduction x@{ name, initialization } -> do
        currentScope <- ask
        if currentScope.isTopLevel
          then ado
            initialization' <- for initialization go
            in AST.VariableIntroduction $ x { initialization = initialization' }
          else ado
            initialization' <-
              for initialization \init ->
                withReaderT
                  (\scope -> scope { bindings = Set.insert name scope.bindings }) $
                  go init
            in AST.VariableIntroduction $ x { initialization = initialization' }
      AST.Block xs -> do
        currentScope <- ask
        xs' <- A.reverse <<< snd <$>
          A.foldM (\(scope /\ asts') ->
            case _ of
              ast@AST.VariableIntroduction { name } | not currentScope.isTopLevel ->
                let
                  scope' =
                    scope { bindings = Set.insert name scope.bindings }
                in do
                  ast' <-
                    withReaderT (const scope') $
                      go ast
                  pure (scope' /\ ast' A.: asts')
              ast@AST.Var var | not currentScope.isTopLevel ->
                let
                  scope' =
                    scope { bindings = Set.insert var scope.bindings }
                in
                  pure (scope' /\ ast A.: asts')
              ast -> ado
                ast' <-
                  withReaderT (const scope) do
                    go ast
                in scope /\ ast' A.: asts'
          ) (currentScope /\ []) xs
        pure $ AST.Block xs'
      AST.App a xs ->
        AST.App <$> go a <*> traverse go xs
      AST.StatementExpression a ->
        AST.StatementExpression <$> go a
      AST.Return a ->
        AST.Return <$> go a
      AST.Assignment x a b ->
        AST.Assignment x <$> go a <*> go b
      AST.Binary i a b ->
        AST.Binary i <$> go a <*> go b
      AST.ArrayLiteral xs ->
        AST.ArrayLiteral <$> traverse go xs
      AST.Indexer a b ->
        AST.Indexer <$> go a <*> go b
      AST.StructLiteral x ->
        AST.StructLiteral <$> traverse go x
      AST.ObjectLiteral xs ->
        AST.ObjectLiteral <$>
          for xs \{ key, value } ->
            { key: _, value: _ }
              <$> go key
              <*> go value
      AST.Cast t ast ->
        AST.Cast t <$> go ast
      AST.Accessor a b ->
        AST.Accessor <$> go a <*> go b
      AST.While a b ->
        AST.While <$> go a <*> go b
      AST.IfElse a b mC ->
        AST.IfElse <$> go a <*> go b <*> traverse go mC
      x ->
        pure x

  eraseLambda { arguments, body } = do
    currentScope <- ask

    -- TODO: reduce this to what is know to actually being used.
    --       objects are considered used if there's an (unshadowed)
    --       reference from anywhere in the lambda's body AST.
    let
      capturedScope =
        currentScope
          { bindings =
              currentScope.bindings
                -- `Set.difference`
                --   (Set.fromFoldable $ A.catMaybes [ currentScope.lhs ])
          }

    -- emit the struct to the top-level
    scopeStruct <- do
      { name, members, ast } <- scopeToStruct capturedScope
      { name, members } <$ tell [ ast ]

    -- assemble a new top-level function that re-assembles the captured
    contFuncName <- ado
      id <- freshId
      in
        moduleName <> "_" <>
          (fromMaybe "anon" currentScope.function) <>
            "__cont_"  <> show id <> "__"

    body' <-
      withReaderT
        (\scope ->
          scope {
            bindings =
              scope.bindings <> Set.fromFoldable (_.name <$> arguments)
          }) $ withReaderT (_ { lhs = Nothing }) $ go body

    tell $ A.singleton $
      AST.Function
        { name: Just contFuncName
        , arguments:
            [ { name: "$ctx"
              , type: Type.Pointer (Type.RawType scopeStruct.name [])
              }
            ] <> arguments
        , returnType: R.any
        , qualifiers: []
        , variadic: true
        , body:
            Just $
              AST.Block $
                (A.catMaybes $
                  A.mapWithIndex <@> scopeStruct.members $ \i varName -> ado
                    guard $ not (A.elem varName $ _.name <$> arguments)
                    in AST.VariableIntroduction
                      { name: varName
                      , type: R.any
                      , qualifiers: []
                      , initialization:
                          Just $
                            AST.Accessor (AST.Var varName) $
                              AST.Var "$ctx"
                      }
                ) <> [ body' ]
        }

    -- build up the continuation context and return it.
    -- since this lambda might have been inlined, we must be sure to operate
    -- in the context of an expression also. This means that we must return
    -- a single AST node that produces the continuation.
    contCtxName <- ado
      id <- freshId
      in "__cont_"  <> show id <> "__"

    pure $ AST.StatementExpression $
      AST.Block $
        [ AST.VariableIntroduction
            { name: "$scope"
            , type: Type.Pointer R.any
            , qualifiers: []
            , initialization:
                Just $
                  AST.App
                    R._purs_scope_alloc $
                    [ AST.NumericLiteral $
                        Left $ A.length scopeStruct.members
                    ]
            }
        , AST.VariableIntroduction
            { name: "$cont"
            , type: R.any
            , qualifiers: []
            , initialization:
                Just $
                  AST.App
                    R.purs_any_cont_new
                      [ AST.Var "$scope"
                      , AST.Cast (Type.Pointer (R.void [ Type.Const ])) $
                          AST.Var contFuncName
                      ]
            }
        ] <>
          (A.mapWithIndex <@> scopeStruct.members $ \i v ->
            AST.Assignment true
              (AST.Indexer (AST.NumericLiteral $ Left i) (AST.Var "$scope")) $
                if Just v == capturedScope.lhs
                  then
                    AST.App R.purs_indirect_thunk_new
                      [ AST.Var "$ivalue" ]
                  else
                    AST.Var v
          ) <>
        [ AST.Var "$cont"
        ]

  scopeToStruct
    :: ∀ n r
     . Applicative n
    => MonadSupply n
    => { function :: Maybe String, bindings :: Set String | r }
    -> n { name :: String, ast :: AST, members :: Array String }
  scopeToStruct { function, bindings } =
    let
      members =
        A.fromFoldable bindings

    in ado
      name <- ado
        id <- freshId
        in
          (fromMaybe "anon" function) <>
            "__scope_" <> show id <> "__"
      in
        { name
        , members
        , ast:
            AST.App
              R._PURS_SCOPE_T
              [ AST.Raw name
              , AST.Block $
                  members <#> \var ->
                    AST.VariableIntroduction
                      { name: var
                      , type: R.any
                      , qualifiers: []
                      , initialization: Nothing
                      }
              ]
        }

-- | Split out variable declarations and definitions on a per-block (scope)
-- | level and hoist the declarations to the top of the scope.
hoistVarDecls :: Array AST -> Array AST
hoistVarDecls = map go
  where
  go =
    AST.everywhere case _ of
      AST.Block xs ->
        AST.Block $
          let
            (decls /\ xs') =
              A.foldl
                (\(decls /\ xs') x ->
                  case x of
                    AST.VariableIntroduction
                      x@{ name
                        , type: typ
                        , initialization: Just initialization
                        } ->
                      let
                        -- TODO: init to 'NULL' only for pointer types.
                        decl =
                          AST.VariableIntroduction $
                            x { initialization = Just R._NULL }
                        isManaged =
                          typ == R.any || typ == R.any'
                        x' =
                          AST.Assignment isManaged
                            (AST.Var x.name)
                            initialization
                      in
                        (decls <> [ (name /\ decl) ]) /\ (xs' <> [ x' ])
                    x ->
                      decls /\ (xs' <> [ x ])
                ) ([] /\ []) xs
          in
            if A.null decls
              then xs
              else
                map snd (A.nubBy (compare `on` fst) decls) <>
                  hoistVarDecls xs'
      x -> x
