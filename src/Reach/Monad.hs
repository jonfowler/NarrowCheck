module Reach.Monad where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Identity
import Reach.Env

data ReachError 
  = DepthLimit
  | RecursiveLimit
  | ConstraintFail

data ReachT m a = Reach 
  { fromReach :: Env -> m (Either ReachError a, Env)
  }

type ReachL  = ReachT []
type Reach  = ReachT Identity

