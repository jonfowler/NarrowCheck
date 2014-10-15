{-# LANGUAGE FlexibleContexts #-}

module Reach.Eval.Basic where

import Reach.Eval


eval :: (Match m, Monad m) => Exp -> ReachT m Exp
eval (Var x) = do
  me <- look x   
  case me of
    Nothing -> return (Var x)
    Just e -> do
      v <- eval e 
      bind x v
      return v

eval (Ap (Fun fid) es) = inlineFun fid es

eval (Ap (Con _ _) _) = throwError (RunTimeError "Constructor in application")

eval (Ap e es) = do
  v <- eval e
  eval $ Ap v es

eval (Case subj alts) = do
  a <- eval subj
  match a alts

eval a = return a

instance Match Identity where
  match (Con cid es) alts = 
  match Target _ = Target
  match _ _ = throwError (RunTimeError "Basic Evaluation: match called with argument which is not a value)

class Match m where
  match :: Exp -> [Alt] -> ReachT m Exp
