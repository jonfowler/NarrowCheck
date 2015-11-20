module Reach.Eval.Basic where

import Reach.Eval.Env
import Reach.Eval.Expr
import Reach.Eval.Monad
import Control.Lens
import Data.DList


eval :: Expr -> ReachT Identity Expr
eval (Let x e e')  = do
  a <- use (env . at x)
  case a of
    Just _ -> error "variable already bound"
    Nothing -> do
      env . at x ?= e 
      eval e'

eval (Var x) = do
  a <- use (env . at x)
  case a of
     Nothing -> error "variable not bound"
     Just e -> do 
       v <- eval e 
       env . at x ?= v
       return v

eval (App f e) = do
  l <- eval f
  case l of
    Lam x e' -> do
      env . at x ?= e
      eval e'
    Con cid es -> return (Con cid (snoc es e))

    _ -> error "function evaluated to non lambda"

--evalApp :: Moand m => Expr -> [Expr] -> ReachT m Expr
       


inlineFunc :: Monad m => FId -> ReachT m Expr
inlineFunc = undefined

--eval 
