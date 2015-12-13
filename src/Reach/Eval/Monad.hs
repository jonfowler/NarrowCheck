{-# LANGUAGE UndecidableInstances #-}
module Reach.Eval.Monad (
  module X,
  ReachT,
  ReachFail(..),
  MonadFork(..),
  Tree(..)
  ) where

import Control.Monad.State as X
import Control.Monad.Except as X
import Control.Monad.Identity as X
import Control.Monad.List

import Data.Maybe

import Reach.Eval.Env

data ReachFail
  = DataLimitFail
  | RecLimitFail
  | ConstraintFail deriving Show

type ReachT m = StateT Env (ExceptT ReachFail m)

class (Monad m) => MonadChoice m where
  memp :: m a
  infixr 4 <|>
  (<|>) :: m a -> m a -> m a


instance MonadChoice [] where
  memp = []
  l1 <|> l2 = l1 ++ l2

instance MonadChoice m => MonadChoice (ExceptT e m) where
  memp = lift memp
  ExceptT l1 <|> ExceptT l2 = ExceptT $ l1 <|> l2 

instance MonadChoice m => MonadChoice (StateT e m) where
  memp = lift memp
  StateT s1 <|> StateT s2 = StateT $ \e -> s1 e <|> s2 e
    

data Tree i t a = Leaf a | Fork i [(t , Tree i t a)]

instance Functor (Tree i t) where
  fmap f (Leaf a) = Leaf $ f a
  fmap f (Fork i as) = Fork i (fmap (fmap (fmap f)) as)

instance Applicative (Tree i t) where
  Leaf f <*> t = f <$> t
  Fork i fs <*> t = Fork i (fmap (fmap (<*> t)) fs)
  pure a = Leaf a

instance Monad (Tree i t) where
  Leaf a >>= f = f a
  Fork i as >>= f = Fork i (fmap (fmap (>>= f)) as)
  return a = Leaf a

class (Monad m) => MonadFork m where
  type SubEff m :: * -> *
  type ForkInfo m
  type ForkTag m
  fork :: ForkInfo m -> [SubEff m (ForkTag m , m a)] -> m a

instance MonadFork (Tree i t) where
  type SubEff (Tree i t) = Identity
  type ForkInfo (Tree i t) = i
  type ForkTag (Tree i t) = t
  fork i ls = Fork i $ map (\(Identity a) -> a) ls

--instance MonadFork (ExceptT e (Tree i t)) where
--  type SubEff (ExceptT e (Tree i t)) = Identity
--  type ForkInfo (ExceptT e (Tree i t)) = i
--  type ForkTag (ExceptT e (Tree i t)) = t
--  fork i ls = ExceptT $ Fork i $ map (\(Identity a) -> fmap runExceptT a) ls

instance (Monad (SubEff m), MonadFork m) => MonadFork (ExceptT e m) where
  type SubEff (ExceptT e m) = SubEff m 
  type ForkInfo (ExceptT e m) = ForkInfo m 
  type ForkTag (ExceptT e m) = ForkTag m 
  fork i ls = ExceptT $ fork i (map (\m -> do (t,a) <- m; return (t, runExceptT a)) ls)

instance (Monad (SubEff m), MonadFork m) =>
               MonadFork (StateT s m) where
  type SubEff (StateT s m) = StateT s (SubEff m)
  type ForkInfo (StateT s m) = ForkInfo m
  type ForkTag (StateT s m) = ForkTag m
  fork i ls = StateT $ \s -> fork
                             i
                             (map (stateFork s) ls)
    where stateFork s (StateT f) = do
                               ((t , f') , s') <- f s
                               return (t , runStateT f' s')

--instance (Monad (SubEff m), MonadFork m) =>
--         MonadFork (ExceptT e m) where
--  type SubEff (ExceptT e m) = ExceptT e (SubEff m)
--  type ForkInfo (ExceptT e m) = ForkInfo m
--  type ForkTag (ExceptT e m) = ForkTag m
--
--  fork i ls = ExceptT $ fork i (mapMaybe exceptFork ls)

--fork :: MonadFork m i t => i -> [m (t , a)] -> m a
--fork i ls = forkInternal i (map (\m -> m >>= ) ls)
