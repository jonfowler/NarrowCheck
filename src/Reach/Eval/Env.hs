{-# LANGUAGE TemplateHaskell #-}

module Reach.Eval.Env (
  Env(..),
  funcs,frees,env,nextVar,funcNames,funcIds,constrNames,
  showExpr)
  where

import Reach.Eval.Expr

import Data.IntMap
import qualified Data.Map as M
import Data.Map (Map)
import Reach.Lens

import qualified Data.DList as D

data Env = Env {
  _funcs :: IntMap Func,
  _frees :: IntMap (CId, [CId]),
  _env :: IntMap Expr,
  _nextVar :: LId,

  _funcNames :: IntMap String,
  _funcIds :: Map String FId,
  _constrNames :: IntMap String
  }

makeLenses ''Env

showExpr :: Expr -> Env -> String
showExpr (Con cid es) env = env ^. constrNames . at' cid ++ showEs (D.toList es) env
showExpr _ _ = "Can't show no constructor value"

showEs :: [Expr] -> Env -> String
showEs [] _ = ""
showEs (e : es) env = " ( " ++ showExpr e env ++ " )" ++ showEs es env



