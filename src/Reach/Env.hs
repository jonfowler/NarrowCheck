{-# LANGUAGE FlexibleContexts #-}

module Reach.Env
  ( Env(..),
    emptyEnv,
    toProg,
    replaceVar
  ) where

import Reach.Syntax
import qualified Data.IntMap as I
import  Data.IntMap (IntMap)
import Data.List
import Control.Monad
import Control.Monad.State
import Control.Applicative
import Data.Generics.Uniplate.Data

data Env = Env
  { _funs :: IntMap Func
  , _env :: IntMap Exp
  , _nextVar :: VarID 
  }

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


replaceVar :: VarID -> Exp -> Exp
replaceVar i = transformBi f
  where f (VarID v) = VarID v + i
        f a = a
