module Language.PureScript.CodeGen.C.Transforms
  ( hoistVarDecls
  , eraseLambdas
  , releaseResources
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Reader (ask, runReaderT, withReaderT)
import Control.Monad.State (execStateT)
import Control.Monad.State as State
import Control.Monad.Writer (runWriterT, tell)
import Data.Array as A
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Function (on)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Set as Set
import Data.Traversable (for, traverse)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Debug.Trace (traceM)
import Language.PureScript.CodeGen.C.AST (AST(..), everywhereTopDown) as AST
import Language.PureScript.CodeGen.C.AST (AST)
import Language.PureScript.CodeGen.C.AST as AST
import Language.PureScript.CodeGen.C.AST as Type
import Language.PureScript.CodeGen.C.AST.Common (isReferenced) as AST
import Language.PureScript.CodeGen.C.Common (isInternalVariable)
import Language.PureScript.CodeGen.C.Pretty as PP
import Language.PureScript.CodeGen.CompileError (CompileError(..))
import Language.PureScript.CodeGen.Runtime as R
import Language.PureScript.CodeGen.SupplyT (class MonadSupply, freshId)

-- | Traverse all blocks, collecting expressions that cause heap allocations and
-- | emitting a corresponding "free"-ing call when the block no longer needs
-- | the variable.
-- |
-- | * We know that 'return;' exists the function immediately with the given
-- |   value.
-- | * We know that purec-generated functions must have at most a single return
-- |   value of type 'ANY'.
-- | * Thus, we must call 'PURS_ANY_RELEASE' on *all* variables, except the
-- |   one returned.
--
-- todo: Could we remove shadowed bindings in a separate pass? keword: SSA
-- idea: use gotos?
--   { var ret; goto end; end: [<cleanup>...]; return ret; }
releaseResources
  :: ∀ m
   . Monad m
  => MonadSupply m
  => MonadError CompileError m
  => Array AST
  -> m (Array AST)
releaseResources = traverse go
  where
  go =
    AST.everywhereTopDownM $ case _ of
      AST.Block xs ->
        AST.Block <<< _.out <$> do
          execStateT <@> { vars: [], out: [] } $ do
            for_ xs $ case _ of
              x@(AST.VariableIntroduction v@{ name }) -> do
                pushVar v
                pushAst x
              x ->
                pushAst x
      x ->
        pure x

  pushAst x = State.modify_ (\s -> s { out = A.snoc s.out x })
  pushVar x = State.modify_ (\s -> s { vars = A.snoc s.vars x })

-- releaseResources = traverse go
--   where
--   go x = execStateT <@> x $
--     pure x


-- | Split out variable declarations and definitions on a per-block (scope)
-- | level and hoist the declarations to the top of the scope.
hoistVarDecls :: Array AST -> Array AST
hoistVarDecls = map go
  where
  go =
    AST.everywhereTopDown case _ of
      AST.Block xs ->
        AST.Block $
          let
            (decls /\ xs') =
              A.foldl
                (\(decls /\ asts) x ->
                  case x of
                    AST.VariableIntroduction
                      x@{ name
                        , type: typ
                        , initialization: Just initialization
                        } ->
                      let
                        -- XXX we may want to initilize to NULL in a seperate
                        --     pass (after inlining), in order to avoid
                        --     dereferencing uninitialized memory.
                        declareVar =
                          AST.VariableIntroduction $
                            x { initialization = Nothing
                              }
                        assignVar =
                          AST.Assignment
                            (AST.Var x.name)
                            initialization
                      in
                        (decls <> [ (name /\ declareVar) ]) /\
                          (asts <> [ assignVar ])
                    x ->
                      decls /\ (asts <> [ x ])
                ) ([] /\ []) xs
          in
            if A.null decls
              then xs
              else
                map snd (A.nubBy (compare `on` fst) decls) <>
                  xs'
      x -> x

-- | Erase lambdas from the AST by capturing used bindings in a heap-allocated,
-- | buffer and emitting a top-level continuation function.
-- |
-- | XXX: we might have to run this pass *after* optimization passes ran in
-- |      order to not capture inlined and unused variables.
eraseLambdas
  :: ∀ m
   . Monad m
  => MonadSupply m
  => MonadError CompileError m
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
          , depth: 0
          , bindings: Set.empty
          }
    in toplevels <> asts'

  where
  go =
    case _ of
      AST.Assignment (AST.Var v) b
        | not (isInternalVariable v) ->
        AST.Assignment (AST.Var v) <$> do
          withReaderT (_ { lhs = Just v }) ado
            b' <- go b
            in
              if AST.isReferenced v b'
                then do
                  AST.StatementExpression $
                    AST.Block
                      [ AST.VariableIntroduction
                          { name: "$_ivalue"
                          , type: Type.Pointer R.any
                          , qualifiers: []
                          , initialization:
                              Just $
                                AST.App R.purs_indirect_value_new []
                          }
                      , AST.VariableIntroduction
                          { name: "$_value"
                          , type: R.any
                          , qualifiers: []
                          , initialization: Just $ b'
                          }
                      , AST.App R.purs_indirect_value_assign
                          [ AST.Var "$_ivalue"
                          , AST.Var "$_value"
                          ]
                      , AST.Var "$_value"
                      ]
                else b'
      AST.Function x@{ name, arguments, body: Just body } ->
        withReaderT (\s -> s { isTopLevel = false, depth = s.depth + 1 }) $
          eraseLambda { arguments, body }
      ast@AST.VariableIntroduction x@{ name, initialization, type: typ } -> do
       currentScope <- ask
       withReaderT (_ { function = Just name }) do
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
              ast@AST.VariableIntroduction { name, type: typ }
                | not currentScope.isTopLevel && not (isInternalVariable name) ->
                let
                  scope' =
                    scope { bindings = Set.insert name scope.bindings }
                in do
                  ast' <-
                    withReaderT (const scope') $
                      go ast
                  pure (scope' /\ ast' A.: asts')
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
      AST.Assignment a b ->
        AST.Assignment <$> go a <*> go b
      AST.Unary i a ->
        AST.Unary i <$> go a
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
      capturedBindings = A.fromFoldable capturedScope.bindings
      capturedScope =
        currentScope
          { bindings =
              Set.fromFoldable $
                A.filter (AST.isReferenced <@> body) $
                  A.filter (not <<< isInternalVariable) $
                    A.fromFoldable $
                      currentScope.bindings
          }

    -- assemble a new top-level function that re-assembles the captured
    contFuncName <- ado
      id <- freshId
      in
        -- XXX: beware that this assumes that `currentScope.function` - if set -
        --      be already fully qualified by `moduleName` in order to avoid
        --      name clashes.
        fromMaybe (moduleName <> "_anon") currentScope.function <>
          "__cont_" <> show currentScope.depth <> "_$"  <> show id

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
            [ { name: "$_ctx"
              , type: Type.Pointer (Type.RawType R.purs_scope_t [ Type.Const ])
              }
            , { name: fromMaybe "$_unused" $ _.name <$> A.head arguments
              , type: R.any
              }
            , { name: "$_va_args"
              , type: Type.RawType "va_list" []
              }
            ]
        , returnType: R.any
        , qualifiers: []
        , variadic: false
        , body:
            Just $
              AST.Block $
                let
                  -- TODO refactor this code to make it easier to follow.
                  --      we essentially recover in-scope bindings and function
                  --      argument bindings by introducing function-local
                  --      bindings. We must ensure that function argument
                  --      bindings shadow in-scope bindings, and that arguments
                  --      recovered from the va_list are recovered in correct
                  --      order.
                  bindings =
                    Map.fromFoldable $
                      A.filter
                        (maybe
                          (const true)
                          (\x -> \v -> notEq (fst v) x)
                          (_.name <$> A.head arguments)
                        ) $
                          A.concat $
                            [ A.mapWithIndex <@> capturedBindings $
                                \offset name ->
                                  name /\ offset /\ Just offset
                            , A.mapWithIndex <@> (fromMaybe [] $ A.tail arguments) $
                                \i { name } ->
                                  name /\ i /\ Nothing
                            ]
                in
                 A.concat
                  [ map snd $ A.sortBy (compare `on` fst) $
                     Map.toUnfoldable bindings <#> \(name /\ i /\ mOffset) ->
                      i /\ case mOffset of
                        Nothing ->
                          AST.VariableIntroduction
                            { name
                            , type: R.any
                            , qualifiers: []
                            , initialization:
                                Just $
                                  AST.App (AST.Var "va_arg")
                                    [ AST.Var "$_va_args"
                                    , AST.Raw $ PP.renderType R.any
                                    ]
                            }
                        Just _ ->
                          AST.VariableIntroduction
                            { name
                            , type: R.any
                            , qualifiers: []
                            , initialization:
                                Just $
                                  AST.App (AST.Var "purs_scope_binding_at")
                                    [ AST.Var "$_ctx"
                                    , AST.NumericLiteral $ Left i
                                    ]
                            }
                  , [ body' ]
                  ]
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
            { name: "$_scope"
            , type: Type.Pointer (Type.RawType R.purs_scope_t [])
            , qualifiers: []
            , initialization:
                Just $
                  if A.null capturedBindings
                    then AST.Null
                    else
                      AST.App
                        R.purs_scope_new1 $
                        [ AST.NumericLiteral $
                            Left $ A.length capturedBindings
                        ]
            }
        , AST.VariableIntroduction
            { name: "$_cont"
            , type: R.any
            , qualifiers: []
            , initialization:
                Just $
                  AST.App
                    R.purs_any_cont
                      [ AST.App
                          R.purs_cont_new
                            [ AST.Var "$_scope"
                            , AST.Cast (Type.Pointer (R.void [ Type.Const ])) $
                                AST.Var contFuncName
                            ]
                      ]
            }
        ] <>
          (A.mapWithIndex <@> capturedBindings $ \i v ->
            AST.App (AST.Var "purs_scope_capture_at")
              [ AST.Var "$_scope"
              , AST.NumericLiteral $ Left i
              , if Just v == capturedScope.lhs
                  then
                    AST.App R.purs_indirect_thunk_new
                      [ AST.Var "$_ivalue" ]
                  else
                    AST.Var v
              ]
          ) <>
        [ AST.App (AST.Var "PURS_RC_RELEASE")
            [ AST.Var "$_scope"
            ]
        , AST.Var "$_cont"
        ]
