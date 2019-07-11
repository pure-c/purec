module Language.PureScript.CodeGen.C.Optimizer.TCO
  ( tco
  ) where

import Prelude

import Data.Array ((:))
import Data.Array as A
import Data.Either (Either(..))
import Data.Foldable (all)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.String as Str
import Data.Tuple.Nested ((/\))
import Language.PureScript.CodeGen.C.AST (AST)
import Language.PureScript.CodeGen.C.AST as AST
import Language.PureScript.CodeGen.Runtime as R

-- | Eliminate tail calls
-- |
-- | Transform a self-recursive function
tco :: AST -> AST
tco = AST.everywhere convert
  where
  convert
    x@(AST.App
       (AST.Var "purs_indirect_value_assign")
       [ v@(AST.Var internalIdent), (fn@AST.Function _)])
    | Just name <- Str.stripPrefix (wrap "$_indirect_") internalIdent =
    let
      args /\ body /\ replace =
        let
          args /\ body /\ replace =
            collectFnArgs fn
        in
          A.concat (A.reverse args) /\ body /\ replace
    in
     if isTailRecursive name body
      then
        AST.App
          (AST.Var "purs_indirect_value_assign")
          [ v
          , replace $ toLoop name args body
          ]
      else x
  convert x = x

  tcoDone = "$_tco_done"
  tcoLoop = "$_tco_loop"
  tcoResult = "$_tco_result"

  copyVar n = "$_copy_" <> n
  copyFnArg a = a { name = copyVar a.name }

  collectFnArgs = go [] identity
    where
    go acc f (AST.Function fn@{ arguments, body: Just (AST.Block sts) })
      | Just { head: body@(AST.Return _) } <- A.uncons sts =
      go (map _.name arguments : acc) <@> body $ \b ->
        f $
          AST.Function $ fn
            { body = Just $ AST.Block [ b ]
            }

    go acc f (AST.Function fn@{ arguments, body: Just (body@(AST.Block _)) }) =
      (map _.name arguments : acc) /\ body /\ \b ->
        f $
          AST.Function $ fn
            { body = Just b
            }

    go acc f (AST.Return (AST.Function fn@{ arguments, body: Just (AST.Block [ body ]) })) =
      go (map _.name arguments : acc) <@> body $ \b ->
        f $
          AST.Return $
            AST.Function $ fn
              { body = Just $ AST.Block [ b ]
              }

    go acc f (AST.Return (AST.Function fn@{ arguments, body: Just body@(AST.Block _) })) =
      (map _.name arguments : acc) /\ body /\ \b ->
        f $
          AST.Return $
            AST.Function $ fn
              { body = Just b
              }

    go acc f body =
      acc /\ body /\ f

  isTailRecursive :: String -> AST -> Boolean
  isTailRecursive ident ast =
    countSelfReferences ast > 0 &&
      allInTailPosition ast

    where
    countSelfReferences = AST.everything (+) match
      where
      match (AST.Var ident') | ident == ident' = 1
      match _ = 0

    allInTailPosition = go
      where
      go (AST.Return expr)
        | isSelfCall ident expr =
            countSelfReferences expr == 1
        | otherwise =
            countSelfReferences expr == 0
      go (AST.While ast body) =
        countSelfReferences ast == 0 &&
          go body
      go (AST.IfElse ast body el) =
        countSelfReferences ast == 0 &&
          go body &&
            all go el
      go (AST.Block body) =
        A.all go body
      go (AST.VariableIntroduction { initialization }) =
        all ((_ == 0) <<< countSelfReferences) initialization
      go (AST.Assignment _ ast) =
        countSelfReferences ast == 0
      go _ = false

  toLoop :: String -> Array String -> AST -> AST
  toLoop ident args ast =
    AST.Block $
      (args <#> \arg ->
          AST.VariableIntroduction
            { name: copyVar arg
            , type: R.any
            , qualifiers: []
            , initialization:
                Just $
                  AST.App R.purs_any_copy
                    [ AST.Var arg ]
            })
      <>
      [ AST.VariableIntroduction
          { name: tcoDone
          , type: R.any
          , qualifiers: []
          , initialization:
              Just $
                AST.App R.purs_any_int
                  [ AST.NumericLiteral $ Left 0
                  ]
          }
      , AST.VariableIntroduction
          { name: tcoResult
          , type: R.any
          , qualifiers: []
          , initialization: Nothing
          }
      , AST.VariableIntroduction
          { name: tcoLoop
          , type: R.any
          , qualifiers: []
          , initialization:
              Just $
                AST.Function
                  { name: Just tcoLoop
                  , arguments:
                      [ { name: tcoDone
                        , type: R.any
                        }
                      ] <> do
                        args <#> \name ->
                          { name: copyVar name
                          , type: R.any
                          }
                  , qualifiers: []
                  , returnType: R.any
                  , variadic:  false
                  , body:
                      Just $
                        AST.Block $
                          (args <#> \arg ->
                              AST.VariableIntroduction
                                { name: arg
                                , type: R.any
                                , qualifiers: []
                                , initialization:
                                    Just $
                                      AST.App R.purs_any_copy
                                        [ AST.Var $ copyVar arg ]
                                })
                          <>
                          [ loopify ast ]
                  }
          }
      , AST.While (AST.Unary AST.Not (AST.App R.purs_any_get_int [ AST.Var tcoDone ])) $
          AST.Block
            [ AST.Assignment (AST.Var tcoResult) $
                AST.App R.purs_any_app $
                  A.concat $
                    [ [ AST.Var tcoLoop, AST.Var tcoDone ]
                    , AST.Var <<< copyVar <$> args
                    , [ AST.Null ]
                    ]
            ]
      , AST.Return $ AST.Var tcoResult
      ]

    where
    loopify :: AST -> AST
    loopify (AST.Return ret)
      | isSelfCall ident ret =
        let
          allArgumentValues =
            collectArguments ret
        in
          AST.Block $
            A.zipWith
              (\val arg ->
                AST.App R.purs_any_assign_mut [ AST.Var $ copyVar arg, val ])
              allArgumentValues
              args
            <>
            [ AST.Return AST.Null ]
      | otherwise =
          AST.Block
            [ markDone
            , AST.Return ret
            ]
    loopify (AST.While cond body) = AST.While cond (loopify body)
    loopify (AST.IfElse cond body el) = AST.IfElse cond (loopify body) (loopify <$> el)
    loopify (AST.Block body) = AST.Block (map loopify body)
    loopify x = x

    markDone =
      AST.App R.purs_any_int_set_mut
        [ AST.Cast R.anyMut $ AST.Var tcoDone
        , AST.NumericLiteral (Left 1)
        ]

    collectArguments = go []
      where
      go acc (AST.App (AST.Var "purs_any_app") args)
        | Just { head: fn, tail: args' } <- A.uncons args =
          go (args' : acc) fn
      go acc _ =
        A.concat acc

  isSelfCall ident (AST.App (AST.Var "purs_any_app") args)
    | Just { head: (AST.Var ident') } <- A.uncons args
    = ident == ident'
  isSelfCall ident (AST.App (AST.Var "purs_any_app") args)
    | Just { head: fn } <- A.uncons args
    = isSelfCall ident fn
  isSelfCall _ _ =
    false
