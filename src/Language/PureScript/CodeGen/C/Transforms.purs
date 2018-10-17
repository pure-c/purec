module Language.PureScript.CodeGen.C.Transforms
  ( hoistVarDecls
  , eraseLambdas
  ) where

import Prelude

import Control.Monad.Reader (ask, runReaderT, withReaderT)
import Control.Monad.Writer (runWriterT, tell)
import Control.MonadPlus (guard)
import Data.Array as A
import Data.Either (Either(..))
import Data.Function (on)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Set as Set
import Data.Traversable (for, traverse)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Language.PureScript.CodeGen.C.AST (AST)
import Language.PureScript.CodeGen.C.AST as AST
import Language.PureScript.CodeGen.C.AST as Type
import Language.PureScript.CodeGen.C.Common (freshName, isInternalVariable)
import Language.PureScript.CodeGen.Runtime as R
import Language.PureScript.CodeGen.SupplyT (class MonadSupply, freshId)

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
              AST.StatementExpression $
                AST.Block
                  -- TODO: only do this dance if self-recursive binding
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
      capturedScope =
        currentScope
          { bindings =
              Set.fromFoldable $
                A.filter (not <<< isInternalVariable) $
                  A.fromFoldable $
                    currentScope.bindings
          }

    -- emit the struct to the top-level
    scopeStruct <- do
      { name, members, ast } <- scopeToStruct capturedScope
      { name, members } <$ tell [ ast ]

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
              , type: Type.Pointer (Type.RawType scopeStruct.name [])
              }
            ] <>
            arguments <>
            [ { name: "$_va_args"
              , type: Type.RawType "va_list" []
              }
            ]
        , returnType: R.any
        , qualifiers: []
        , variadic: false
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
                              AST.Var "$_ctx"
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
            { name: "$_scope"
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
            { name: "$_cont"
            , type: R.any
            , qualifiers: []
            , initialization:
                Just $
                  AST.App
                    R.purs_any_cont_new
                      [ AST.Var "$_scope"
                      , AST.Cast (Type.Pointer (R.void [ Type.Const ])) $
                          AST.Var contFuncName
                      ]
            }
        ] <>
          (A.mapWithIndex <@> scopeStruct.members $ \i v ->
            AST.Assignment
              (AST.Indexer (AST.NumericLiteral $ Left i) (AST.Var "$_scope")) $
                if Just v == capturedScope.lhs
                  then
                    AST.App R.purs_indirect_thunk_new
                      [ AST.Var "$_ivalue" ]
                  else
                    AST.Var v
          ) <>
        [ AST.Var "$_cont"
        ]

  scopeToStruct
    :: ∀ n r
     . Applicative n
    => MonadSupply n
    => _
    -> n { name :: String, ast :: AST, members :: Array String }
  scopeToStruct currentScope =
    let
      members =
        A.fromFoldable currentScope.bindings

    in ado
      name <- ado
        id <- freshId
        in
          fromMaybe (moduleName <> "_anon") currentScope.function <>
            "__cont_" <> show currentScope.depth <> "_$"  <> show id
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
