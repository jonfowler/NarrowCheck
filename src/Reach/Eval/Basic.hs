module Reach.Eval.Basic where

import Reach.Eval.Env
import Reach.Eval.Expr
import Reach.Eval.Monad
import Control.Lens


eval :: Expr -> ReachT Identity Expr
eval (Let x e e')  = do
  a <- use (env . at x)
  case a of
    Just _ -> error "variable already bound"
    Nothing -> do
      env . at x ?= e 
      eval e'



--eval 
