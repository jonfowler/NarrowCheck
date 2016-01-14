module Reach.Eval.Env (
  Env(..),
  funcs,
  free, nextFVar, freeDepth, maxDepth, topFrees,
  env, nextEVar, nextLVar,
  funcNames, funcIds, constrNames, constrIds,
  showAtom, printFVar)
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
  _nextFVar :: !FId,
  _freeDepth :: IntMap Int,
  _maxDepth :: !Int,
  _topFrees :: [FId],

  _env :: IntMap Expr,
  _nextEVar :: !EId,
  _nextLVar :: !LId,

  _funcNames :: IntMap String,
  _funcIds :: Map String FuncId,
  _constrNames :: IntMap String,
  _constrIds :: Map String CId
  } deriving Show

makeLenses ''Env

showAtom :: Env -> Atom -> String
showAtom env (Con cid es) = env ^. constrNames . at' cid ++ bracket (map (showAtom env) es)
showAtom _ e = "Can't show non constructor value: " ++ show e

bracket :: [String] -> String
bracket = foldr (\x s -> " (" ++ x ++ ")" ++ s) ""


printFVar :: Env -> FId ->  String
printFVar env x = case env ^. free . at x of
  Just (cid, xs) -> env ^. constrNames . at' cid ++ bracket (map (printFVar env) xs)
  Nothing -> "_"
