module Reach.Eval.Forward where

import Reach.Eval.Basic
import Reach.Eval


instance Match [] where
  match (Con cid es) alts = case findAlt cid alts of
    Just (Alt _ vs e) -> do
      zipWithM_ bind vs es
      return e
    Nothing -> throwError (RunTimeError "Incomplete Case Expression")
  match Target _ = return Target

  match (Var x) alts = do
    a <- look x
    case a of
      Just _ -> throwError (RunTimeError "Variable bound in match statement")
      Nothing -> do
        i <- getD x
        a <- getMaxD
        if i < a 
        then do  
          alt <- lift alts
          e <- bindAlt x (i+1) alt
          eval e 
        else throwError DepthLimit
  match _ _ = throwError (RunTimeError "Basic Evaluation: match called with argument which is not a variable/value")

bindAlt :: VarID -> Int -> Alt -> ReachT [] Exp
bindAlt x i (Alt cid xs e) = do
  bind x (Con cid (map Var xs))
  mapM_ (flip bindD i) xs
  return e
  
evalF :: Exp -> Env -> [(Either ReachError Exp, Env)]
evalF e s = runReach (eval e) s

normalF :: Exp -> Env -> [(Either ReachError Exp, Env)]
normalF e s = runReach (normal e) s

reachF :: Exp -> Env -> [ Env ]
reachF e s = [ s | (Right Target, s) <- normalF e s ]

