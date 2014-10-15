{-# LANGUAGE FlexibleContexts #-}

module Reach.Env
  ( Env(..),
    emptyEnv,
    toProg,
    findF,
    lookupV,
    insertV
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
  , _nextVar :: VarID 
  }

lookupV :: VarID -> Env -> Maybe Exp 
lookupV (VarID v) = I.lookup v . _env

insertV :: VarID -> Exp -> Env -> Env
insertV (VarID v) e s = Env {_env = I.insert v e $ _env s}

findF :: FunID -> Env -> Func 
findF (FunID f) = (I.! f) . _funs

emptyEnv :: Env
emptyEnv = Env
  { _funs = I.empty
  , _env = I.empty
  , _nextVar = 0
  }

ffid :: Func -> Int 
ffid = fromFunID . fid

toProg :: [Func] -> (Exp, Env)
toProg m = case find ((=="main") . name) m of
  Just a -> let funcs = I.fromList $ map (\a -> (ffid a, a)) m
   in (Fun $ fid (funcs I.! ffid a) , emptyEnv {_funs = funcs })
  Nothing -> error "no main"



