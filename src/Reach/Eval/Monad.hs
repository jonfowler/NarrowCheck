module Reach.Eval.Monad (
  module X,
  ReachT,
  ReachFail(..),
  MonadChoice(..),
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

type ReachT m = StateT (Env Expr) (ExceptT ReachFail m)

data Tree a = Leaf a | Branch [Tree a] deriving Functor

instance Applicative Tree where
  pure = Leaf
  Leaf f <*> t = f <$> t
  Branch fs <*> t = Branch $ fmap (<*> t) fs

instance Monad Tree where
  return = Leaf
  Leaf a >>= m = m a
  Branch ts >>= m = Branch $ fmap (>>= m) ts

class (Monad m) => MonadChoice m where
  memp :: m a
--  infixr 4 <|>
--  (<|>) :: m a -> m a -> m a
  mchoice :: MonadChoice m => [m a] -> m a


--mchoice = foldr (<|>) memp

          

instance MonadChoice [] where
  memp = []
  mchoice = concat 

instance MonadChoice m => MonadChoice (ExceptT e m) where
  memp = lift memp
  mchoice es = ExceptT $ mchoice (runExceptT <$> es)
--  ExceptT l1 <|> ExceptT l2 = ExceptT $ l1 <|> l2 

instance MonadChoice m => MonadChoice (StateT e m) where
  memp = lift memp
  mchoice ss = StateT $ \s -> mchoice (($ s) . runStateT <$> ss)
    
