
module Reach.Eval.Env (
  Env(..),
  funcs,free,nextFVar,env,nextEVar,funcNames,funcIds,constrNames,
  showExpr, printFVar)
  where

import Reach.Eval.Expr

import qualified Data.IntMap as I
import Data.IntMap (IntMap)
import qualified Data.Map as M
import Data.Map (Map)
import Reach.Lens

data Env = Env {
  _funcs :: IntMap Func,

  _free :: IntMap (CId, [FId]),
  _nextFVar :: FId,

  _env :: IntMap Expr,
  _nextEVar :: LId,

  _funcNames :: IntMap String,
  _funcIds :: Map String FuncId,
  _constrNames :: IntMap String
  } deriving Show

makeLenses ''Env

showExpr :: Env -> Expr -> String
showExpr env (Con cid es) = env ^. constrNames . at' cid ++ bracket (map (showExpr env) es)
showExpr _ e = "Can't show non constructor value: " ++ show e

bracket :: [String] -> String
bracket = foldr (\x s -> " (" ++ x ++ ")" ++ s) ""


printFVar :: Env -> FId ->  String
printFVar env x = case env ^. free . at x of
  Just (cid, xs) -> env ^. constrNames . at' cid ++ bracket (map (printFVar env) xs)
  Nothing -> "_"
