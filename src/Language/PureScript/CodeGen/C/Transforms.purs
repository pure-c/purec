module Language.PureScript.CodeGen.C.Transforms
  ( eraseLambdas
  , releaseResources
  , staticStrings
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Reader (ask, runReaderT, withReaderT)
import Control.Monad.State (runStateT)
import Control.Monad.State as State
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (runWriterT, tell)
import Control.MonadPlus (guard)
import Data.Array as A
import Data.Either (Either(..))
import Data.Function (on)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Set as Set
import Data.String as Str
import Data.Traversable (for, traverse)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Language.PureScript.CodeGen.C.AST (AST(..), Type(..), ValueQualifier(..), everywhere, everywhereM)
import Language.PureScript.CodeGen.C.AST (AST(..), Type, everywhereTopDown, FunctionQualifier(..)) as AST
import Language.PureScript.CodeGen.C.AST as Type
import Language.PureScript.CodeGen.C.AST.Common (isReferenced) as AST
import Language.PureScript.CodeGen.C.Common (freshInternalName', isInternalVariable, prefixInternalVar)
import Language.PureScript.CodeGen.C.Optimizer.Blocks (collapseNestedBlocks)
import Language.PureScript.CodeGen.C.Pretty as PP
import Language.PureScript.CodeGen.CompileError (CompileError)
import Language.PureScript.CodeGen.Runtime as R
import Language.PureScript.CodeGen.SupplyT (class MonadSupply, freshId)

staticStrings
  :: ∀ m
   . Monad m
  => MonadSupply m
  => MonadError CompileError m
  => Array AST
  -> m (Array AST)
staticStrings xs = do
  asts /\ strs <- runWriterT (traverse (everywhereM convert) xs)
  pure $
    (strs <#> \(name /\ s) ->
      VariableIntroduction
          { name
          , type: RawType R.purs_str_t []
          , qualifiers: []
          , initialization:
              Just
                (App (Var "purs_str_static_lazy") [StringLiteral s])
          }
    ) <> asts

  where
  convert (App (Var "purs_str_static_new") [ StringLiteral s ]) = do
    n <- freshInternalName' "static_str"
    tell [ n /\ s ]
    pure (App R.purs_address_of [Var n])
  convert x =
    pure x

-- | Generate code that releases intermediate results.
-- |
-- | Step 0: Flatten all nested blocks. We should throw an 'InternalError'
-- |         if a nested block is found. The reason is that nested blocks have
-- |         different scoping and may return early.
-- | Step 1: Identify all AST.App calls causing resource allocation, and extract
-- |         those into fresh (internal) variables.
-- | Step 2: Extract all intermediate 'purs_any_app' results to temporary
-- |         fresh (internal) variables.
-- | Step 3: Bind the return value to a temporary 'ANY' value and call
-- |         PURS_ANY_RETAIN on it. This allows us to safely perform the next
-- |         step.
-- | Step 4: Release all previously found resource acquistions/intermediate
-- |         'purs_any_app' results with PURS_RC_RELEASE/PURS_ANY_RELEASE
-- |         respectively, except the return value bound in Step 3.
-- | Step 5: Return the result bound in Step 3.
-- |
-- | Considerations:
-- |   * Consider that some branches in a block may return early. In such case,
-- |     simply free everything we learned about up to that point and return.
-- |
-- | Future work:
-- | * Currently we need to allocate a stack variable for every capturing every
-- |   temporary variable. However, since 'ANY' values are fat, this might
-- |   cause pressure on available stack memory for sufficiently large programs
-- |   (speculating here.) We could look into a way to improve upon this,
-- |   perhaps by building a more intelligent dependency graph and reducing the
-- |   number of introduced variables on the stack.
releaseResources
  :: ∀ m
   . Monad m
  => MonadSupply m
  => MonadError CompileError m
  => Array AST
  -> m (Array AST)
releaseResources = map (map cleanup) <<< traverse (go [])
  where
  cleanup =
    everywhere case _ of
      AST.Block xs ->
        case A.unsnoc xs of
          Just { init, last } ->
            AST.Block $
              (init # A.filter case _ of
                AST.Var _ -> false
                _ -> true
              ) <> [ last ]
          Nothing ->
            AST.Block xs
      x -> x

  -- note: we purposely omit 'purs_scope_t' as that one is fully managed by
  --       the 'eraseLambdas' transform.
  allocatedType = case _ of
    AST.Var "purs_any_app"            -> Just R.any
    AST.Var "purs_vec_new_va"         -> Just arrayType
    AST.Var "purs_vec_copy"           -> Just arrayType
    AST.Var "purs_vec_splice"         -> Just arrayType
    AST.Var "purs_vec_concat"         -> Just arrayType
    AST.Var "purs_str_static_new"     -> Just stringType
    AST.Var "purs_str_new"            -> Just stringType
    AST.Var "purs_foreign_new"        -> Just foreignType
    AST.Var "purs_record_new_va"      -> Just recordType
    AST.Var "purs_cont_new"           -> Just contType
    AST.Var "purs_cons_new"           -> Just consType
    AST.Var "purs_any_lazy_new"       -> Just R.any
    AST.Var "purs_record_add_multi"   -> Just recordType
    AST.Var "purs_any_force_array"    -> Just arrayType
    AST.Var "purs_any_force_record"   -> Just recordType
    AST.Var "purs_any_force_cons"     -> Just consType
    _                                 -> Nothing

  go parentVars = case _ of
    AST.Block xs -> do
      -- build up a new block, collect new variables we should introduce, and
      -- indicate whether or not the block has returned. The latter is necessary
      -- to ensure we are still releasing temporary variables introduced in the
      -- block, as they are about to leave the scope.
      out /\ { allocVars, hasReturned } <- do
        runStateT <@> { allocVars: [], hasReturned: false } $
          let
            go' =
              case _ of
                -- enter a new context. the block is - itself - responsible
                -- for clean up.
                x@(AST.Block _) -> do
                  { allocVars } <- State.get
                  lift $ go (parentVars <> allocVars) x

                -- deal with potential resource allocations, which are *always*
                -- due to applying some function.
                -- we capture the result of the function in a fresh, internal
                -- name that we can release later.
                x@(AST.App n args) -> do
                  n'    <- go' n
                  args' <- traverse go' args
                  case allocatedType n' of
                    Just typ -> do
                      name' <- lift $ freshInternalName' "fnret"
                      State.modify_ \state ->
                        state
                          { allocVars =
                               A.snoc state.allocVars
                                { name: name'
                                , type: typ
                                }
                          }
                      pure $
                        AST.StatementExpression $
                          AST.Block
                            [ AST.Assignment (AST.Var name') (AST.App n' args')
                            , AST.Var name'
                            ]
                    Nothing ->
                      pure $ AST.App n' args'

                -- avoid entering a new isolated context for statement
                -- expressions, since they might not return and can be used as
                -- rhs values to purs_any_app, etc.
                AST.StatementExpression (AST.Block xs') ->
                  AST.StatementExpression <<< AST.Block <$> traverse go' xs'

                -- deal with returning. we must release all variables we
                -- collected, *including* any variables collected in our parent
                -- scopes, since we won't be coming back.
                AST.Return x -> do
                  let
                    escapeRet = case x of
                      AST.App (AST.Var "purs_tco_state_result") [_] -> true
                      _ -> false
                  x' <- if escapeRet then pure  x else go' x
                  { allocVars } <- State.get
                  State.modify_ (_ { hasReturned = true })
                  tmp  <- lift $ freshInternalName' "ret"
                  pure $
                    AST.Block $
                      (if escapeRet then [] else
                        [ AST.VariableIntroduction
                            { name: tmp
                            , type: R.any
                            , qualifiers: []
                            , initialization: Just x'
                            }
                        , AST.App (AST.Var "PURS_ANY_RETAIN")
                          [ AST.Var tmp
                          ]
                        ])
                      <>
                      ((parentVars <> allocVars) <#> \v ->
                        if v.type == R.any
                          then
                            AST.App (AST.Var "PURS_ANY_RELEASE")
                              [ AST.Var v.name
                              ]
                          else
                            AST.App (AST.Var "PURS_RC_RELEASE")
                              [ AST.Var v.name
                              ]
                      )
                      <>
                      [ AST.Return if escapeRet then x else AST.Var tmp
                      ]

                AST.VariableIntroduction x ->
                  -- XXX do not recurse into scope assignments!
                  --     this assumption is tightly coupled to the
                  --     'eraseLambdas' transforms.
                  if x.type == scopeType
                    then pure $ AST.VariableIntroduction x
                    else ado
                      initialization' <- traverse go' x.initialization
                      in AST.VariableIntroduction $
                          x { initialization = initialization'
                            }
                AST.Unary o a ->
                  AST.Unary o <$> go' a
                AST.Binary o a b ->
                  AST.Binary o <$> go' a <*> go' b
                AST.ArrayLiteral as ->
                  AST.ArrayLiteral <$> traverse go' as
                AST.Indexer a b ->
                  AST.Indexer <$> go' a <*> go' b
                AST.ObjectLiteral as ->
                  AST.ObjectLiteral <$> for as \{ key, value } -> ado
                    key'   <- go' key
                    value' <- go' value
                    in { key: key', value: value' }
                AST.Accessor a b ->
                  AST.Accessor <$> go' a <*> go' b
                AST.Cast a b ->
                  AST.Cast a <$> go' b
                AST.Assignment a b ->
                  AST.Assignment <$> go' a <*> go' b
                AST.While a b ->
                  AST.While <$> go' a <*> go' b
                AST.IfElse a b c ->
                  AST.IfElse <$> go' a <*> go' b <*> traverse go' c
                x ->
                  pure x
          in
            traverse go' xs
      pure $
        AST.Block $
          A.concat $
            [ allocVars <#> \var ->
                AST.VariableIntroduction
                  { name: var.name
                  , type: var.type
                  , qualifiers: []
                  , initialization: Nothing
                      -- Just
                      --   if var.type == R.any
                      --     then AST.Var "purs_any_null"
                      --     else AST.Var "NULL"
                  }
            , out # A.filter case _ of
                AST.Var _ -> false
                _         -> true
            , -- if the last statement was a AST.Var, we were likely in a
              -- statement expression. therefore, we must retain it's value
              -- before freeing resources introduced in the scope.
              fromMaybe [] $
                case A.last out of
                  Just (x@(AST.Var name)) ->
                    Just
                      [ AST.App (AST.Var "PURS_ANY_RETAIN")
                          [ AST.Var name ]
                      , x
                      ]
                  _ -> Nothing
            , if not hasReturned
                then
                  -- we're leaving scope, release all vars introduced in this
                  -- scope before leaving it.
                  (allocVars <#> \v ->
                    if v.type == R.any
                      then
                        AST.App (AST.Var "PURS_ANY_RELEASE")
                          [ AST.Var v.name
                          ]
                      else
                        AST.App (AST.Var "PURS_RC_RELEASE")
                          [ AST.Var v.name
                          ]
                  ) <>
                  -- if the last statement was a AST.Var, we were likely in a
                  -- statement expression. therefore, make sure we place it back
                  -- at the end of the block.
                  maybe [] A.singleton
                    case A.last out of
                      Just (x@(AST.Var _)) -> Just x
                      _                    -> Nothing
                else
                  []
            ]

    -- top-level variable introductions.
    AST.VariableIntroduction v@{ type: Any _, initialization: Just x@(AST.Block _) } -> do
      ast' <- go parentVars x
      pure $ AST.VariableIntroduction $ v { initialization = Just ast' }

    -- top-level block-less variable introductions.
    -- we turn those into statement expressions.
    AST.VariableIntroduction v@{ type: Any _, initialization: Just x } -> do
      tmp  <- freshInternalName' "alloc"
      ast' <-
        go parentVars $
          AST.Block
            [ AST.VariableIntroduction $ v { name = tmp }
            , AST.Var tmp
            ]
      pure $
        AST.VariableIntroduction $
          v
            { initialization =
                Just $
                  AST.StatementExpression ast'
            }

    AST.Function f@{ body: Just x@(AST.Block _) } -> ado
      ast' <- go parentVars x
      in AST.Function $ f { body = Just ast' }
    x ->
      pure x

-- | Erase lambdas from the AST by capturing used bindings into a scope data.
-- | structure.
-- |
-- | XXX: we might have to run this pass *after* optimization passes ran in
-- |      order to not capture inlined and unused variables.
-- |
-- | todo: does the inner lambda need to retain it's bound scope and arguments
-- |       during every execution?
eraseLambdas
  :: ∀ m
   . Monad m
  => MonadSupply m
  => MonadError CompileError m
  => String -- ^ lambda prefix
  -> Array AST
  -> m (Array AST)
eraseLambdas moduleName asts = map collapseNestedBlocks <$>
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
                          { name: prefixInternalVar "ivalue"
                          , type: Type.Pointer R.any
                          , qualifiers: []
                          , initialization:
                              Just $
                                AST.App R.purs_any_ref_new []
                          }
                      , AST.VariableIntroduction
                          { name: prefixInternalVar "value"
                          , type: R.any
                          , qualifiers: []
                          , initialization: Just $ b'
                          }
                      , AST.App R.purs_any_ref_write
                          [ AST.Var (prefixInternalVar "ivalue")
                          , AST.Var (prefixInternalVar "value")
                          ]
                      , AST.Var (prefixInternalVar "value")
                      ]
                else b'
      AST.Function x@{ name, arguments, body: Just body } -> do
        currentScope <- ask
        withReaderT (\s -> s { isTopLevel = false, depth = s.depth + 1 }) $
          eraseLambda currentScope.isTopLevel { arguments, body }
      ast@(AST.VariableIntroduction x@{ name, initialization, type: typ }) -> do
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
              -- for mutually recursive bindings we cannot rely on variable
              -- introductions and have to look for the output of a previous
              -- transform which assigns variables through a mutable ref.
              -- Unfortunately that means un-mangling the name...
              -- See the 'C.Rec' handling in C.purs.
              ast@(AST.App (AST.Var "purs_any_ref_write") [AST.Var mangledName, AST.Function _])
                | Just name <- Str.stripPrefix (Str.Pattern (prefixInternalVar "ref_")) mangledName -- See C.purs
                , not currentScope.isTopLevel ->
                  let
                    scope' =
                      scope
                        { function = Just name
                        , bindings = Set.insert name scope.bindings
                        }
                  in ado
                    ast' <-
                      withReaderT (const scope') $
                        go ast
                    in (scope /\ ast' A.: asts')
              ast@(AST.VariableIntroduction { name, type: typ, initialization })
                | not currentScope.isTopLevel && not (isInternalVariable name) ->
                case initialization of
                  Just (AST.Function fn) ->
                    let
                      scope' =
                        scope
                          { function = fn.name
                          , bindings = Set.insert name scope.bindings
                          }
                    in ado
                      ast' <-
                        withReaderT (const scope') $
                          go ast
                      in (scope' /\ ast' A.: asts')
                  _ ->
                    let
                      scope' =
                        scope { bindings = Set.insert name scope.bindings }
                    in ado
                      ast' <-
                        withReaderT (const scope') $
                          go ast
                      in (scope' /\ ast' A.: asts')
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

  eraseLambda isTopLevel { arguments, body } = do
    currentScope <- ask

    let
      capturedBindings = A.fromFoldable capturedScope.bindings
      capturedScope =
        currentScope
          { bindings =
              Set.fromFoldable $
                A.filter (\name -> currentScope.function /= Just name) $
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
            function = Nothing,
            bindings =
              maybe scope.bindings (Set.insert <@> scope.bindings)
                currentScope.function <>
                  Set.fromFoldable (_.name <$> arguments)
          }) $ withReaderT (_ { lhs = Nothing }) $ go body

    tell $ A.singleton $
      AST.Function
        { name: Just contFuncName
        , arguments:
            [ { name: prefixInternalVar "ctx"
              , type: Type.Pointer (Type.RawType R.purs_scope_t [ Type.Const ])
              }
            , { name: fromMaybe (prefixInternalVar "unused") $ _.name <$> A.head arguments
              , type: R.any
              }
            , { name: prefixInternalVar "va_args"
              , type: Type.RawType "va_list" []
              }
            ]
        , returnType: R.any
        , qualifiers: [ AST.ModuleInternal ]
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
                  [ A.catMaybes
                      [ do
                        fnName <- currentScope.function
                        guard (not isTopLevel)
                        guard (AST.isReferenced fnName body')
                        pure $
                          AST.VariableIntroduction
                            { name: fnName
                            , type: R.any
                            , qualifiers: []
                            , initialization:
                                Just $
                                  AST.App (AST.Var "purs_any_cont")
                                    [ AST.App (AST.Var "purs_cont_new")
                                        [ AST.Var (prefixInternalVar "ctx")
                                        , AST.Var contFuncName
                                        ] ]
                            }
                      ]
                  , map snd $ A.sortBy (compare `on` fst) $
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
                                    [ AST.Var (prefixInternalVar "va_args")
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
                                    [ AST.Var (prefixInternalVar "ctx")
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

    if A.null capturedBindings
      then pure $
        AST.App
          R.purs_any_cont
            [ AST.App
                R.purs_cont_new
                  [ AST.Null
                  , AST.Var contFuncName
                  ]
            ]
      else ado
        scopeVarName <- freshInternalName' "scope"
        contVarName  <- freshInternalName' "cont"
        in AST.StatementExpression $
          AST.Block
            [ AST.VariableIntroduction
                { name: scopeVarName
                , type: scopeType
                , qualifiers: []
                , initialization:
                    Just $
                      AST.App
                          R.purs_scope_new $
                          [ AST.NumericLiteral $ Left $ A.length capturedBindings ]
                          <>
                          (capturedBindings <#> \v ->
                            -- todo: solve this dilemma (recursion?)
                            -- todo: do we need to retain these? should this be
                            --       handeled by 'releaseResources' transform?
                            if Just v == capturedScope.lhs
                              then
                                AST.App R.purs_any_lazy_new
                                  [ AST.Var (prefixInternalVar "ivalue") ]
                              else
                                AST.Var v
                          )
                }
            , AST.VariableIntroduction
                { name: contVarName
                , type: R.any
                , qualifiers: []
                , initialization:
                    Just $
                      AST.App
                        R.purs_any_cont
                          [ AST.App
                              R.purs_cont_new
                                [ AST.Var scopeVarName
                                , AST.Var contFuncName
                                ]
                          ]
                }
            , AST.App (AST.Var "PURS_RC_RELEASE") [ AST.Var scopeVarName ]
            , AST.Var contVarName
            ]

consType :: AST.Type
consType = Type.Pointer (Type.RawType "purs_cons_t" [ Type.Const ])

contType :: AST.Type
contType = Type.Pointer (Type.RawType "purs_cont_t" [ Type.Const ])

recordType :: AST.Type
recordType = Type.Pointer (Type.RawType "purs_record_t" [ Type.Const ])

foreignType :: AST.Type
foreignType = Type.Pointer (Type.RawType "purs_foreign_t" [ Type.Const ])

stringType :: AST.Type
stringType = Type.Pointer (Type.RawType "purs_str_t" [ Type.Const ])

arrayType :: AST.Type
arrayType = Type.Pointer (Type.RawType "purs_vec_t" [ Type.Const ])

scopeType :: AST.Type
scopeType = Type.Pointer (Type.RawType "purs_scope_t" [ Type.Const ])
