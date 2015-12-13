{-# LANGUAGE FlexibleContexts #-}

module Reach.Eval.Lazy where

import Reach.Eval.Gen
import Reach.Eval.Expr
import Reach.Eval.Monad
import Reach.Eval.Env
import Reach.Lens

import Control.Monad
import qualified Data.DList as D

matchLazy :: (MonadFork m, ForkInfo m ~ FId, ForkTag m ~ (CId, [FId]), Monad (SubEff m)) => Match m  
matchLazy (Con _ _) [] = error "no match for constructor in case"
matchLazy (Con cid es) (Alt cid' xs e : as)
  | cid == cid' = binds xs (D.toList es) e
  | otherwise   = matchBasic (Con cid es) as
matchLazy (FVar x) as = fork x (map (branchAlt x) as)
matchLazy e _ = error $ "case subject did not evaluate to constructor: " ++ show e

branchAlt :: (MonadFork m, ForkInfo m ~ FId, ForkTag m ~ (CId, [FId]), Monad (SubEff m)) =>
             FId -> Alt -> StateT Env (SubEff m) ((CId, [FId]) , ReachT m Expr)
branchAlt x (Alt cid vs e) = do
  (xs, e') <- bindToFrees vs e
  free . at x ?= (cid, xs)
  return ((cid, xs), return e')


bindToFrees :: Monad m => [LId] -> Expr -> StateT Env m ([FId], Expr)
bindToFrees [] e = return ([] , e)
bindToFrees (v : vs) e = do
  (x, e') <- bindToFree v e
  (xs, e'') <- bindToFrees vs e'
  return (x : xs, e'')
                          
bindToFree :: Monad m => LId -> Expr -> StateT Env m (FId, Expr)
bindToFree v e = do
  x <- use nextFVar
  nextFVar += 1
  return (x , replaceLVar v (FVar x) e)
 


       
