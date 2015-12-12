module Reach.Eval.Lazy where

import Reach.Eval.Gen
import Reach.Eval.Expr
import Reach.Eval.Monad

import Control.Monad
import qualified Data.DList as D

matchLazy :: MonadChoice m => Match m
matchLazy (Con _ _) [] = error "no match for constructor in case"
matchLazy (Con cid es) (Alt cid' xs e : as)
  | cid == cid' = binds xs (D.toList es) e
  | otherwise   = matchBasic (Con cid es) as
matchLazy (FVar x) [] = memp
matchLazy (FVar x) (Alt cid' vs e : as) = _
matchLazy e _ = error $ "case subject did not evaluate to constructor: " ++ show e



       
