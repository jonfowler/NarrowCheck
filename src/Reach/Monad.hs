{-# LANGUAGE MultiParamTypeClasses #-}

module Reach.Monad
  ( module Control.Monad.State,
    module Control.Monad.Except,
    ReachT,
    ReachL,
    Reach
  ) where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Identity
import Reach.Env

data ReachError 
  = DepthLimit
  | RecursiveLimit
  | ConstraintFail

newtype ReachT m a = Reach 
  { fromReach :: Env -> m (Either ReachError a, Env)
  }

type ReachL  = ReachT []
type Reach  = ReachT Identity

instance Monad m => Monad (ReachT m) where
  return a = Reach $ \env -> return (Right a, env)
  (Reach m) >>= g = Reach $ \env -> do 
      (err, env2) <- m env 
      case err of
        Right a -> fromReach (g a) env2
        Left e -> return (Left e, env2)

instance Monad m => MonadState Env (ReachT m) where
  get = Reach $ \env -> return (Right env, env)
  put env = Reach $ \_ -> return (Right (), env)
  state f = Reach $ \env -> let (a, env2) = f env in return (Right a, env2)

instance Monad m => MonadError ReachError (ReachT m) where
  throwError e = Reach $ \env -> return (Left e, env)
  catchError (Reach m) handler = Reach $ \env -> do
    (a, env2) <- m env
    case a of
      Left e -> fromReach (handler e) env2
      Right a -> return (Right a, env2)
    
