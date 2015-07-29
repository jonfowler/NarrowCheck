{-# LANGUAGE FlexibleContexts #-}

module Reach.Env
  ( Env(..),
    emptyEnv,
    toProg,
    findF,
    lookupV,
    insertV,
    lookupD,
    insertD,
    getExp
  ) where

import Reach.Syntax
import qualified Data.IntMap as I
import  Data.IntMap (IntMap)
import Data.List
import Control.Monad
import Control.Monad.State
import Control.Applicative

data Env = Env
  { _funs :: IntMap Func
  , _env :: IntMap Exp
  , _varDepth :: IntMap Int
  , _nextVar :: VarID 
  , maxd :: Int
  , maxf :: Int
  }

lookupV :: VarID -> Env -> Maybe Exp 
lookupV (VarID v) = I.lookup v . _env

lookupD :: VarID -> Env -> Maybe Int 
lookupD (VarID v) = I.lookup v . _varDepth

insertV :: VarID -> Exp -> Env -> Env
insertV (VarID v) e s = s {_env = I.insert v e $ _env s}

insertD :: VarID -> Int -> Env -> Env
insertD (VarID v) i s = s {_varDepth = I.insert v i $ _varDepth s}

findF :: FunID -> Env -> Func 
findF (FunID f) = (I.! f) . _funs

emptyEnv :: Env
emptyEnv = Env
  { _funs = I.empty
  , _env = I.empty
  , _varDepth = I.empty
  , _nextVar = 0
  , maxd = 10000000000
  , maxf = 10000000000
  }

ffid :: Func -> Int 
ffid = fromFunID . fid

toProg :: [Func] -> Int -> Int -> (Exp, [VarID], Env)
toProg m dd fd = case find ((=="main") . name) m of
  Just a -> let funcs = I.fromList $ map (\a -> (ffid a, a)) m
   in (Ap (Fun $ fid (funcs I.! ffid a)) [] , args a,
       envD {_funs = funcs, maxd = dd, maxf = fd })
    where envD = foldr (flip insertD 0) emptyEnv $ args a
  Nothing -> error "no main"

getExp :: Env -> VarID -> Exp
getExp s x = getExp' s (Var x)

getExp' :: Env -> Exp -> Exp
getExp' s (Var x) = case lookupV x s of
  Just e -> getExp' s e
  Nothing -> Var x
getExp' s (Con cid es) = Con cid $ map (getExp' s) es
getExp' s a = a

