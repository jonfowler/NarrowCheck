module Overlap.Eval.Monad (
  module X,
  OverlapT,
  runOverlapT,
  runOverlap,
  Overlap,
  OverlapFail(..),
  MonadChoice(..),
  nullT,
  List,
  list,
  runList,
  TreeF(..),
  Tree,
  OverlapTree(..),
  MapTree,
  intoMapTree
  ) where

import Control.Monad.State as X
import Control.Monad.Writer as X hiding (Alt) 
import Control.Monad.Except as X
import Control.Monad.Identity as X
import Control.Monad.Reader as X
import Control.Monad.List as X

import Control.Monad.Free as X

import Control.Arrow

import Overlap.Eval.Env
import Overlap.Eval.Expr
import Overlap.Lens

data OverlapFail
  = DataLimitFail
  | RecLimitFail
  | ConstraintFail deriving Show

type OverlapT m = ExceptT OverlapFail (StateT (Env Expr) m)
type Overlap = OverlapT Identity

runOverlapT :: OverlapT m a -> Env Expr -> m (Either OverlapFail a, Env Expr)
runOverlapT = runStateT . runExceptT

runOverlap :: Overlap a -> Env Expr -> (Either OverlapFail a, Env Expr)
runOverlap o = runIdentity . runOverlapT o

type List = ListT Identity

list :: [a] -> List a
list = ListT . Identity

runList :: List a -> [a]
runList = runIdentity . runListT

data TreeF f a = Leaf a | Branch [f (TreeF f a)] deriving Functor
type Tree = TreeF ((,) Int)

data OverlapTree a = OverlapTree {runOverlapTree :: (Overlap (Either [(Int, OverlapTree a)] a))}

instance Functor OverlapTree where
  fmap f = OverlapTree .  fmap ((map . fmap . fmap) f +++ f) . runOverlapTree

--instance Applicative OverlapTree where
--  pure = OverlapTree . return . Right
--  OverlapTree f <*> OverlapTree g = 
--
--instance Monad OverlapTree where
--  return = OverlapTree . return . Right

type MapTree a = Tree (OverlapTree a, Env Expr -> Env Expr)

intoMapTree :: OverlapTree a -> MapTree a
intoMapTree s = Leaf (s, id)

peelTree :: OverlapTree a -> Env Expr -> (Either ([(Int, OverlapTree a)]) (Either OverlapFail a), Env Expr)
peelTree o = first (either (Right . Left) (fmap Right))  . runOverlap (runOverlapTree o)

instance Functor f => Applicative (TreeF f) where
  pure = Leaf
  Leaf f <*> t = f <$> t
  Branch fs <*> t = Branch ((fmap . fmap)  (<*> t) fs)

instance Functor f => Monad (TreeF f) where
  return = Leaf
  Leaf a >>= m = m a
  Branch ts >>= m = Branch ((fmap . fmap) (>>= m) ts)

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
    
