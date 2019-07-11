module Language.PureScript.CodeGen.C.Transforms
  ( hoistVarDecls
  , eraseLambdas
  ) where

import Prelude

import Control.Monad.Reader (ask, runReaderT, withReaderT)
import Control.Monad.Writer (runWriterT, tell)
import Data.Array as A
import Data.Either (Either(..))
import Data.Function (on)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Set as Set
import Data.Traversable (for, traverse)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Language.PureScript.CodeGen.C.AST (AST)
import Language.PureScript.CodeGen.C.AST (AST(..), everywhereTopDown) as AST
import Language.PureScript.CodeGen.C.AST as Type
import Language.PureScript.CodeGen.C.AST.Common (isReferenced) as AST
import Language.PureScript.CodeGen.C.Common (isInternalVariable)
import Language.PureScript.CodeGen.C.Pretty as PP
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
      capturedScope =
        currentScope
          { bindings =
              Set.fromFoldable $
                A.filter (AST.isReferenced <@> body) $
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
                            [ A.mapWithIndex <@> scopeStruct.members $
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
                                  AST.Accessor (AST.Var name) $
                                    AST.Var "$_ctx"
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
            , type: Type.Pointer R.any
            , qualifiers: []
            , initialization:
                Just $
                  if A.null scopeStruct.members
                    then AST.Null
                    else
                      AST.App
                        R.purs_malloc_many $
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
                    R.purs_any_cont
                      [ AST.Var "$_scope"
                      , AST.NumericLiteral $
                          Left $ Set.size capturedScope.bindings
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
