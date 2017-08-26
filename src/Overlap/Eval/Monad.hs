module Overlap.Eval.Monad (
  module X,
  OverlapT,
  OverlapFail(..),
  MonadChoice(..),
  nullT,
  Tree(..)
  ) where

import Control.Monad.State as X
import Control.Monad.Writer as X hiding (Alt) 
import Control.Monad.Except as X
import Control.Monad.Identity as X
import Control.Monad.List

import Data.Maybe

import Overlap.Eval.Env
import Overlap.Eval.Expr
import Overlap.Lens

data OverlapFail
  = DataLimitFail
  | RecLimitFail
  | ConstraintFail deriving Show

type OverlapT m = ExceptT OverlapFail (StateT (Env Expr) m)

data Tree a = Leaf a | Branch [(Int, Tree a)] deriving Functor

instance Applicative Tree where
  pure = Leaf
  Leaf f <*> t = f <$> t
  Branch fs <*> t = Branch (mapped . _2 %~ (<*> t) $ fs)

instance Monad Tree where
  return = Leaf
  Leaf a >>= m = m a
  Branch ts >>= m = Branch (mapped . _2 %~ (>>= m) $ ts)

nullT :: Tree a -> Bool
nullT (Branch []) = True
nullT _ = False

class (Monad m) => MonadChoice m where
  memp :: m a
--  infixr 4 <|>
--  (<|>) :: m a -> m a -> m a
--  mchoice = foldr (<|>) memp
  mchoice :: MonadChoice m => [(Int, m a)] -> m a

instance MonadChoice Tree where
  memp = Branch []
  mchoice ts = Branch ts

--instance MonadChoice [] where
--  memp = []
----  (<|>) = (++)
--  mchoice = concat 

instance MonadChoice m => MonadChoice (ExceptT e m) where
  memp = lift memp
  mchoice es = ExceptT $ mchoice (mapped . _2 %~ runExceptT $ es) 
  --(runExceptT <$> es)
--  ExceptT l1 <|> ExceptT l2 = ExceptT $ l1 <|> l2 

instance MonadChoice m => MonadChoice (StateT e m) where
  memp = lift memp
  mchoice ss = StateT $ \s -> mchoice (mapped . _2 %~ (($ s) . runStateT) $ ss)
    --(($ s) . runStateT <$> ss)
--  StateT s1 <|> StateT s2 = StateT $ \s -> s1 s <|> s2 s
    
