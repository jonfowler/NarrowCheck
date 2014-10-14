{-# LANGUAGE FlexibleContexts #-}

module Reach.Eval.Basic where

import Reach.Syntax
import Reach.Env
import Control.Monad.State
import Control.Monad.Except

eval :: MonadState Env m => Exp -> m Exp
eval ( = do
  

