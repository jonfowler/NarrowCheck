{-# LANGUAGE FlexibleContexts #-}

module Reach.Env where

import Reach.Syntax
import qualified Data.IntMap as I
import  Data.IntMap (IntMap)
import Data.List
import Control.Monad
import Control.Monad.State
import Control.Applicative
import Data.Generics.Uniplate.Data

data Env = Env
  { funs :: IntMap Func
  , env :: IntMap Exp
  , nextVar :: Int
  }

emptyEnv :: Env
emptyEnv = Env
  { funs = I.empty
  , env = I.empty
  , nextVar = 0
  }

toProg :: [Func] -> (Exp, Env)
toProg m = case find ((=="main") . name) m of
  Just a -> let funcs = I.fromList $ map (\a -> (fid a, a)) m
   in (Fun $ fid (funcs I.! fid a) , emptyEnv {funs = funcs })
  Nothing -> error "no main"

bind :: (MonadState Env m) => VarID -> Exp -> m ()
bind v e = modify (\s -> s { env = I.insert v e $ env s}) 

inlineFun :: (MonadState Env m, Functor m) => FunID -> [Exp] -> m Exp 
inlineFun fid es = do
  s <- get
  let f = funs s I.! fid
  let nv = nextVar s
  put (s {nextVar = nv + varNum f})
  zipWithM_ bind (map (nv+) $ args f) es
  return (replaceVar nv $ body f)
 
replaceVar :: Int -> Exp -> Exp
replaceVar i = transformBi g . transform f
  where f (Var v) = Var $ v + i
        f (Lam v e) = Lam (v+i) e
        f a = a
        g (Alt c vs e) = Alt c (map (i+) vs) e
