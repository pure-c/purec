module Language.PureScript.CodeGen.C.Optimizer.Common.MagicDo
  ( magicDo
  ) where

import Prelude

import Data.Array as A
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Debug.Trace (trace)
import Language.PureScript.CodeGen.C.AST (AST(..), FunctionQualifier(..), UnaryOperator(..))
import Language.PureScript.CodeGen.C.AST as AST
import Language.PureScript.CodeGen.C.Optimizer.Common (isDict)
import Language.PureScript.CodeGen.Runtime as R
import Language.PureScript.Constants as C

-- Inline type class dictionaries for >>= and return for the Eff monad
--
-- E.g.
--
--  Prelude[">>="](dict)(m1)(function(x) {
--    return ...;
--  })
--
-- becomes
--
--  function __do {
--    var x = m1();
--    ...
--  }
magicDo :: AST -> AST
magicDo = AST.everywhereTopDown convert
  where
  -- The name of the function block which is added to denote a do block
  fnName = "__do"
  effectModule = "Effect"
  -- Desugar monomorphic calls to >>= and return for the Eff monad
  convert :: AST -> AST

  -- Desugar pure
  convert
    (App (Var "purs_any_app") [
      (App (Var "purs_any_app") [ pure', val ]),
      purs_any_null ])
      | isPure pure' = val
  convert
    (App (Var "purs_any_app") [ pure', val ])
      | isPure pure' =
        -- TODO: Explore a cont-less 'Cont'. It must retain it's value upon
        --       evaluation to play nice with the rest of the system.
        Function
          { name: Nothing
          , arguments: []
          , body: Just (Return val)
          , qualifiers: [ModuleInternal]
          , variadic: false
          , returnType: R.any
          }

  -- Desugar discard
  convert
    (App (Var "purs_any_app")
      [ App (Var "purs_any_app") [bind, m]
      , Function (fn@{ name: Nothing
                     , body: Just (Block js)
                     })
      ]) | isDiscard bind =
    Function $
      fn { name = Just fnName
         , body = Just
            (Block
              (App R.purs_any_app [m, R.purs_any_null] A.:
                map applyReturns js))
         }
  -- Desugar bind
  convert
    (App (Var "purs_any_app")
      [ App (Var "purs_any_app") [bind, m]
      , Function (fn@{ name: Nothing
                     , arguments: [arg]
                     , body: Just (Block js)
                     })
      ]) | isBind bind =
    Function $
      fn { name = Just fnName
         , arguments = []
         , body = Just
            (Block
              ((VariableIntroduction
                { name: arg.name
                , type: arg.type
                , qualifiers: []
                , initialization: Just (App R.purs_any_app [m, R.purs_any_null])
                }) A.: map applyReturns js))
         }

  -- Desugar untilE
  -- TODO: need to release resources at end of while loop; cannot enable this
  --       pass until we do, or there's memory leak.
  -- convert
  --   (App (Var "purs_any_app")
  --     [ App (Var "purs_any_app") [f, arg]
  --     , Var "purs_any_null"
  --     ]) | isEffectModule C.untilE f =
  --   App R.purs_any_app
  --     [ Function
  --        { name: Nothing
  --        , variadic: false
  --        , qualifiers: [ModuleInternal]
  --        , returnType: R.any
  --        , arguments: []
  --        , body: Just (Block [
  --           While
  --             (Unary Not
  --               (App R.purs_any_force_int
  --                 [ App R.purs_any_app [arg, R.purs_any_null]
  --                 ]))
  --             (Block []),
  --           Return R.purs_any_null
  --         ])
  --        }
  --     , R.purs_any_null
  --     ]

  -- Desugar whileE
  -- TODO: need to release resources at end of while loop; cannot enable this
  --       pass until we do, or there's memory leak.
  -- convert
  --   (App (Var "purs_any_app")
  --     [ App (Var "purs_any_app")
  --         [ App (Var "purs_any_app") [ f, arg1 ]
  --         , arg2
  --         ]
  --     , Var "purs_any_null"
  --     ]) | isEffectModule C.whileE f =
  --   App R.purs_any_app
  --     [ Function
  --       { name: Nothing
  --       , variadic: false
  --       , qualifiers: [ModuleInternal]
  --       , returnType: R.any
  --       , arguments: []
  --       , body: Just
  --           (Block
  --             [ While (App R.purs_any_force_int [ App R.purs_any_app [arg1, R.purs_any_null] ])
  --               (Block [ App R.purs_any_app [arg2, R.purs_any_null] ])
  --             , Return R.purs_any_null
  --             ])
  --       }
  --     , R.purs_any_null
  --     ]

  -- Inline __do returns
  convert
    (Return
      (App (Var "purs_any_app")
        [ Function { name: Just ident, arguments: [], body: Just body }
        , Var "purs_any_null"
        ])) | ident == fnName = body

  -- Inline double applications
  convert
    (App (Var "purs_any_app")
      [ App (Var "purs_any_app")
          [ Function (fn@{ name: Nothing
                         , arguments: []
                         , body: Just (Block body)
                         })
          , Var "purs_any_null" ]
      , Var "purs_any_null"
      ]) =
    App R.purs_any_app
      [ Function (fn { body = Just (Block (applyReturns `map` body)) })
      , R.purs_any_null
      ]

  convert other = other

  -- Check if an expression represents a monomorphic call to >>= for the Eff monad
  isBind (App (Var "purs_any_app") [fn, dict])
    | isDict (effectModule /\ C.effectDictionaries.bindDict) dict &&
      isBindPoly fn = true
  isBind _ = false

  -- Check if an expression represents a call to @discard@
  isDiscard (App (Var "purs_any_app") [ App (Var "purs_any_app") [fn, dict1], dict2])
    | isDict (C.controlBind /\ C.discardUnitDictionary) dict1 &&
      isDict (effectModule /\ C.effectDictionaries.bindDict) dict2 &&
      isDiscardPoly fn = true
  isDiscard _ = false

  -- Check if an expression represents a monomorphic call to pure or return for
  -- the Effect applicative
  isPure (App (Var "purs_any_app") [ fn, dict ])
    | isDict (effectModule /\ C.effectDictionaries.applicativeDict) dict &&
      isPurePoly fn = true
  isPure _ = false

  -- Check if an expression represents the polymorphic >>= function
  isBindPoly = isDict (C.controlBind /\ C.bind)
  -- Check if an expression represents the polymorphic pure function
  isPurePoly = isDict (C.controlApplicative /\ C.pure')
  -- Check if an expression represents the polymorphic discard function
  isDiscardPoly = isDict (C.controlBind /\ C.discard)
  -- Check if an expression represents a function in the Effect module
  isEffectModule name (Var name') =
    ("Effect_" <> name <> "_$") == name'
  isEffectModule _ _ = false

  applyReturns :: AST -> AST
  applyReturns (Return ret) = Return (App R.purs_any_app [ret, R.purs_any_null])
  applyReturns (Block jss) = Block (map applyReturns jss)
  applyReturns (While cond js) = While cond (applyReturns js)
  -- applyReturns (For ss v lo hi js) = For ss v lo hi (applyReturns js)
  -- applyReturns (ForIn ss v xs js) = ForIn ss v xs (applyReturns js)
  applyReturns (IfElse cond t f) = IfElse cond (applyReturns t) (applyReturns `map` f)
  applyReturns other = other
