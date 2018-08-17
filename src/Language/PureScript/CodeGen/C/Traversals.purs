module Language.PureScript.CodeGen.C.Traversals
  ( everythingOnAST
  ) where

import Prelude

import Control.Monad.Writer (execWriter, runWriterT, tell)
import Data.Array as A
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse_)
import Data.Tuple (fst, snd)
import Language.PureScript.CodeGen.C.AST (AST)
import Language.PureScript.CodeGen.C.AST as AST

-- everywhereOnAST :: (AST -> AST) -> AST -> AST
-- everywhereOnAST f = go
--   where
--   go :: AST -> AST
--   go (AST.Unary op j) = f (AST.Unary op (go j))
--   go (AST.Binary op j1 j2) = f (AST.Binary op (go j1) (go j2))
--   go (AST.ArrayLiteral cpp) = f (AST.ArrayLiteral (map go cpp))
--   go (AST.Indexer j1 j2) = f (AST.Indexer (go j1) (go j2))
--   go (AST.DictLiteral cpps) = f (AST.DictLiteral (map (fmap go) cpps))
--   go (AST.Accessor prop j) = f (AST.Accessor (go prop) (go j))
--   go (AST.Function name args rty qs j) = f (AST.Function name args rty qs (go j))
--   go (AST.Lambda cps args rty j) = f (AST.Lambda cps args rty (go j))
--   go (AST.Cast t cpp) = f (AST.Cast t (go cpp))
--   go (AST.DictGet j1 j2) = f (AST.DictGet (go j1) (go j2))
--   go (AST.DataGet j1 j2) = f (AST.DataGet (go j1) (go j2))
--   go (AST.RecordGet k j) = f (AST.RecordGet k (go j))
--   go (AST.App j cpp) = f (AST.App (go j) (map go cpp))
--   go (AST.Block cpp) = f (AST.Block (map go cpp))
--   go (AST.Struct name cpp) = f (AST.Struct name (map go cpp))
--   go (AST.VariableIntroduction name qs j) = f (AST.VariableIntroduction name qs (fmap go j))
--   go (AST.Assignment j1 j2) = f (AST.Assignment (go j1) (go j2))
--   go (AST.While j1 j2) = f (AST.While (go j1) (go j2))
--   go (AST.IfElse j1 j2 j3) = f (AST.IfElse (go j1) (go j2) (fmap go j3))
--   go (AST.Switch cpp cpps d) = f (AST.Switch (go cpp) (map (fmap go) cpps) (fmap go d))
--   go (AST.Return cpp) = f (AST.Return (go cpp))
--   go (AST.Throw cpp) = f (AST.Throw (go cpp))
--   go (AST.Comment com j) = f (AST.Comment com (go j))
--   go other = f other

-- everywhereOnASTTopDown :: (AST -> AST) -> AST -> AST
-- everywhereOnASTTopDown f = runIdentity <<< everywhereOnASTTopDownM (Identity <<< f)

-- everywhereOnASTTopDownM
--   :: Applicative m
--   => Monad m
--   => (AST -> m AST)
--   -> AST
--   -> m AST
-- everywhereOnASTTopDownM f = f >=> go
--   where
--   f' = f >=> go
--   go (AST.Unary op j) = AST.Unary op <$> f' j
--   go (AST.Binary op j1 j2) = AST.Binary op <$> f' j1 <*> f' j2
--   go (AST.ArrayLiteral cpp) = AST.ArrayLiteral <$> traverse f' cpp
--   go (AST.Indexer j1 j2) = AST.Indexer <$> f' j1 <*> f' j2
--   go (AST.DictLiteral cpps) = AST.DictLiteral <$> traverse (pairM f' f') cpps
--   go (AST.Accessor prop j) = AST.Accessor prop <$> f' j
--   go (AST.Function name args rty qs j) = AST.Function name args rty qs <$> f' j
--   go (AST.Lambda cps args rty j) = AST.Lambda cps args rty <$> f' j
--   go (AST.Cast t cpp) = AST.Cast t <$> f' cpp
--   go (AST.DictGet j1 j2) = AST.DictGet <$> f' j1 <*> f' j2
--   go (AST.DataGet j1 j2) = AST.DataGet <$> f' j1 <*> f' j2
--   go (AST.RecordGet k j) = AST.RecordGet k <$> f' j
--   go (AST.App j cpp) = AST.App <$> f' j <*> traverse f' cpp
--   go (AST.Block cpp) = AST.Block <$> traverse f' cpp
--   go (AST.Struct name cpp) = AST.Struct name <$> traverse f' cpp
--   go (AST.VariableIntroduction name qs j) = AST.VariableIntroduction name qs <$> traverse f' j
--   go (AST.Assignment j1 j2) = AST.Assignment <$> f' j1 <*> f' j2
--   go (AST.While j1 j2) = AST.While <$> f' j1 <*> f' j2
--   go (AST.IfElse j1 j2 j3) = AST.IfElse <$> f' j1 <*> f' j2 <*> traverse f' j3
--   go (AST.Switch cpp cpps d) = AST.Switch <$> f' cpp <*> traverse (pairM f' f') cpps <*> traverse f' d
--   go (AST.Return j) = AST.Return <$> f' j
--   go (AST.Throw j) = AST.Throw <$> f' j
--   go (AST.Comment com j) = AST.Comment com <$> f' j
--   go other = f other

everythingOnAST :: ∀ r. Monoid r => (AST -> r) -> AST -> r
everythingOnAST f = execWriter <<< go
  where
  go ast =
    tell (f ast) *>
      traverse_ go case ast of
        AST.Unary _ j1 -> [ j1 ]
        AST.Binary _ j1 j2 -> [ j1, j2 ]
        AST.ArrayLiteral j1 -> j1
        AST.Indexer j1 j2 -> [ j1, j2 ]
        AST.ObjectLiteral keyvals ->
          (map _.key   keyvals) <>
          (map _.value keyvals)
        AST.Accessor j1 j2 -> [ j1, j2 ]
        AST.Function { body } -> A.catMaybes [ body ]
        AST.Lambda { body }-> [ body ]
        AST.Cast _ j1 -> [ j1 ]
        AST.DictGet j1 j2 -> [ j1, j2 ]
        AST.DataGet j1 j2 -> [ j1, j2 ]
        AST.RecordGet _ j1 -> [ j1 ]
        AST.App j1 j2s -> j1 A.: j2s
        AST.Block j1s -> j1s
        AST.Struct _ j1s -> j1s
        AST.VariableIntroduction { initialization: Just j1 } -> [ j1 ]
        AST.Assignment j1 j2 -> [ j1, j2 ]
        AST.While j1 j2 -> [ j1, j2 ]
        AST.IfElse j1 j2 Nothing -> [ j1, j2 ]
        AST.IfElse j1 j2 (Just j3) -> [ j1, j2, j3 ]
        AST.Switch j1 j2s mj3 ->
          j1
            A.: map fst j2s
            <> map snd j2s
            <> maybe [] A.singleton mj3
        AST.Return j1 -> [ j1 ]
        AST.Comment _ j1 -> [ j1 ]
        _ -> []
