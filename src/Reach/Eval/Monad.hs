
module Reach.Eval.Monad (
  module X,
  ReachT,
  ReachFail(..)
  ) where

import Control.Monad.State as X
import Control.Monad.Except as X
import Control.Monad.Identity as X

import Reach.Eval.Env

data ReachFail
  = DataLimitFail
  | RecLimitFail
  | ConstraintFail deriving Show

type ReachT m = StateT Env (ExceptT ReachFail m)



