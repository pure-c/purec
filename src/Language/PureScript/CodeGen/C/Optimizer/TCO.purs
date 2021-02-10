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
import Language.PureScript.CodeGen.C.AST as Type
import Language.PureScript.CodeGen.C.Common (prefixInternalVar)
import Language.PureScript.CodeGen.Runtime as R

-- | Eliminate tail calls
-- |
-- | Transform a self-recursive function
tco :: AST -> AST
tco = AST.everywhere convert
  where
  convert
    x@(AST.App
       (AST.Var "purs_any_ref_write")
       [ v@(AST.Var internalIdent), (fn@(AST.Function _))
       ])
    | Just name <- Str.stripPrefix (wrap (prefixInternalVar ("ref_"))) internalIdent =
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
          (AST.Var "purs_any_ref_write")
          [ v
          , replace $ toLoop name args body
          ]
      else x
  convert x = x

  tcoState = prefixInternalVar "tco_state"
  tcoLoop = prefixInternalVar "tco_loop"
  tcoResult = prefixInternalVar "tco_result"

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
      [ AST.VariableIntroduction
          { name: tcoState
          , type: Type.RawType "struct tco_state" []
          , qualifiers: []
          , initialization:
              Just $
                AST.App (AST.Var "purs_tco_state_new")
                  [ AST.NumericLiteral $ Left $ A.length args
                  ]
          }
      ] <>
      (args # A.mapWithIndex \i arg ->
        AST.App (AST.Var "purs_tco_set_arg")
          [ AST.Var tcoState
          , AST.NumericLiteral $ Left i
          , AST.Var arg
          ])
      <>
      [ AST.VariableIntroduction
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
                      [ { name: tcoState
                        , type: R.any
                        }
                      ]
                  , qualifiers: []
                  , returnType: R.any
                  , variadic:  false
                  , body:
                      Just $
                        AST.Block $
                          (args # A.mapWithIndex \i arg ->
                              AST.VariableIntroduction
                                { name: arg
                                , type: R.any
                                , qualifiers: []
                                , initialization:
                                    Just $
                                      AST.App (AST.Var "purs_tco_get_arg")
                                        [ AST.App (AST.Var "purs_foreign_get_data")
                                            [ AST.App R.purs_any_unsafe_get_foreign
                                                [ AST.Var tcoState
                                                ]
                                            ]
                                        , AST.NumericLiteral $ Left i
                                        ]
                                })
                          <>
                          [ loopify ast ]
                  }
          }
      , AST.While
          (AST.Unary AST.Not
            (AST.App
              (AST.Var "purs_tco_is_done")
              [ AST.Var tcoState
              ])) $
          AST.Block
            [ AST.Assignment (AST.Var tcoResult) $
                AST.App R.purs_any_app
                  [ AST.Var tcoLoop
                  , AST.App R.purs_any_foreign
                      [ AST.App R.purs_foreign_new
                          [ AST.Null
                          , AST.App R.purs_address_of [ AST.Var tcoState ]
                          , AST.Null
                          ]
                      ]
                  ]
            ]
      , AST.App (AST.Var "purs_tco_state_free") [ AST.Var tcoState ]
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
              (\val (i /\ _) ->
                AST.App
                  (AST.Var "purs_tco_mut_arg")
                  [ AST.App (AST.Var "purs_foreign_get_data")
                      [ AST.App R.purs_any_unsafe_get_foreign
                          [ AST.Var tcoState
                          ]
                      ]
                  , AST.NumericLiteral $ Left i
                  , val
                  ])
              allArgumentValues
              (args # A.mapWithIndex (/\))
            <>
            [ AST.Return R.purs_any_null ]
      | otherwise =
          AST.Block
            [ AST.App (AST.Var "purs_tco_set_done")
                [ AST.App (AST.Var "purs_foreign_get_data")
                    [ AST.App R.purs_any_unsafe_get_foreign
                        [ AST.Var tcoState
                        ]
                    ]
                ]
            , AST.Return ret
            ]
    loopify (AST.While cond body) = AST.While cond (loopify body)
    loopify (AST.IfElse cond body el) = AST.IfElse cond (loopify body) (loopify <$> el)
    loopify (AST.Block body) = AST.Block (map loopify body)
    loopify x = x

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
    | Just { head } <- A.uncons args
    = isSelfCall ident head
  isSelfCall _ _ =
    false
