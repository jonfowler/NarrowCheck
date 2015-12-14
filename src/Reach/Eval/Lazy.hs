{-# LANGUAGE FlexibleContexts #-}

module Reach.Eval.Lazy where

import Reach.Eval.Gen
import Reach.Eval.Expr
import Reach.Eval.Monad
import Reach.Eval.Env
import Reach.Lens

import Control.Monad
import qualified Data.DList as D

--evalLazy :: Expr -> ReachT (Tree FId (CId, [FId])) Expr
--evalLazy = evalGen matchLazy

type Choose m = FId -> [(CId, Int)] -> ReachT m (CId, Int)

evalLazy :: MonadChoice m => Expr -> ReachT m Expr
evalLazy = evalGen (matchLazy chooseSimple)
     

matchLazy :: Monad m => Choose m -> Match m  
matchLazy choose (FVar x) as = do
  (cid,vs) <- choose x (map (\(Alt c vs e) -> (c,length vs)) as)
  xs <- newFVars vs
  free . at x ?= (cid, xs)
  matchBasic (Con cid (D.fromList $ map FVar xs)) as
matchLazy _ e as = matchBasic e as

chooseSimple :: MonadChoice m => Choose m
chooseSimple _ as = foldr (<|>) memp (map return as)

newFVar :: Monad m => StateT Env m FId
newFVar = do
  x <- use nextFVar
  nextFVar += 1
  return x

newFVars :: Monad m => Int -> StateT Env m [FId]
newFVars n = sequence (replicate n newFVar)


--bindToFrees :: Monad m => [LId] -> Expr -> StateT Env m ([FId], Expr)
--bindToFrees [] e = return ([] , e)
--bindToFrees (v : vs) e = do
--  (x, e') <- bindToFree v e
--  (xs, e'') <- bindToFrees vs e'
--  return (x : xs, e'')
                          
 


       
