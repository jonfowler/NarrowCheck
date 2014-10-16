{-# LANGUAGE FlexibleContexts #-}

module Reach.Eval.Basic where

import Reach.Eval
import Debug.Trace


eval :: (Match m, Monad m) => Exp -> ReachT m Exp
eval (Var x) = do
  me <- look x   
  case me of
    Nothing -> return (Var x)
    Just e -> do
      v <- eval e 
      bind x v
      return v

eval (Ap (Fun fid) es) = inlineFun fid es >>= eval

eval (Ap (Con _ _) _) = throwError (RunTimeError "Constructor in application")

eval (Ap e es) = do
  v <- eval e
  eval $ Ap v es

eval (Case subj alts) = do
  a <- eval subj
  e <- match a alts 
  eval e

eval a = return a


normal :: (Match m, Monad m) => Exp -> ReachT m Exp
normal Target = return Target
normal (Con cid es)  = do
  es' <- mapM normal es
  if null [ 0 | Target <- es']
  then return $ Con cid es'
  else return Target
normal e = eval e >>= normal

instance Match Identity where
  match (Con cid es) alts = case findAlt cid alts of
    Just (Alt _ vs e) -> do
      zipWithM_ bind vs es
      return e
    Nothing -> throwError (RunTimeError "Incomplete Case Expression")
  match Target _ = return Target
  match _ _ = throwError (RunTimeError "Basic Evaluation: match called with argument which is not a value")

class Match m where
  match :: Exp -> [Alt] -> ReachT m Exp

evalB :: Exp -> Env -> (Either ReachError Exp, Env)
evalB e s = runIdentity $ runReach (eval e) s

normalB :: Exp -> Env -> (Either ReachError Exp, Env)
normalB e s = runIdentity $ runReach (normal e) s
