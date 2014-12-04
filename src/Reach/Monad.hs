{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Reach.Monad
  ( module X,
    ReachT,
    ReachL,
    Reach,
    ReachError(..),
    runReach
  ) where

--import Control.Monad.Except as X
import Control.Monad.State as X
import Control.Monad.Identity as X
import Control.Monad.Trans as X

import Reach.Env


data ReachError 
  = DepthLimit
  | RunTimeError String
  | RecursiveLimit
  | ConstraintFail
  deriving Show

newtype ReachT m a = Reach 
  { runReach :: Env -> m (Either ReachError a, Env)
  }

type ReachL  = ReachT []
type Reach  = ReachT Identity

instance Monad m => Monad (ReachT m) where
  return a = Reach $ \env -> return (Right a, env)
  (Reach m) >>= g = Reach $ \env -> do 
      (err, env2) <- m env 
      case err of
        Right a -> runReach (g a) env2
        Left e -> return (Left e, env2)

instance Monad m => MonadState Env (ReachT m) where
  get = Reach $ \env -> return (Right env, env)
  put env = Reach $ \_ -> return (Right (), env)
  state f = Reach $ \env -> let (a, env2) = f env in return (Right a, env2)

class Monad m => MonadError e m | m -> e where
  throwError :: e -> m a
  catchError :: m a -> (e -> m a) -> m a

instance Monad m => MonadError ReachError (ReachT m) where
  throwError e = Reach $ \env -> return (Left e, env)
  catchError (Reach m) handler = Reach $ \env -> do
    (a, env2) <- m env
    case a of
      Left e -> runReach (handler e) env2
      Right a -> return (Right a, env2)

instance MonadTrans ReachT where
  lift l = Reach $ \env -> liftM (\a -> (Right a, env)) l
    
