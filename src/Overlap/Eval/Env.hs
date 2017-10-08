module Overlap.Eval.Env
--       (
--  Env(..),
--  Expr(..),
--  Atom(..),
--  FullAlts,
--  Alts,
--  atom,
--  toExpr,
--  funcs, funcArgTypes,
--  free, nextFVar, freeDepth, freeType, maxDepth, topFrees, freeCount, maxFreeCount,
--  env, nextEVar, nextLVar, typeConstr,
--  funcNames, funcIds, constrNames, constrIds,
--  showAtom, printFVar, printFVar1)
  where

import Overlap.Eval.Expr

import qualified Data.IntMap as I
import Data.IntMap (IntMap)
import qualified Data.Map as M
import Data.Map (Map)
import Overlap.Lens
import Control.DeepSeq
import GHC.Generics

data Env a = Env {
  _defs :: IntMap ([Int], Def),
  _defArgTypes :: IntMap [Type],

  _free :: IntMap (CId, [FId]),
  _nextFVar :: !FId,
  _freeDepth :: IntMap Int,
  _freeType :: IntMap Type,
  _freeNarrowSet :: IntMap NarrowSet,

  _freeCount :: Int,

  _maxDepth :: !Int,
  _maxFreeCount :: Int,
  _topFrees :: [FId],

  _env :: IntMap a,
--  _nextEVar :: !EId,
  _nextLVar :: !LId,

  _typeConstr :: IntMap [(CId, Int, [TypeExpr])],

  _funcNames :: IntMap String,
  _funcIds :: Map String FId,
  _constrNames :: IntMap String,
  _constrIds :: Map String CId,
  _typeNames :: IntMap String,
  _typeIds :: Map String Int
  } deriving (Functor, Generic)

makeLenses ''Env

instance NFData a => NFData (Env a)

--showAtom :: Env Expr -> Atom -> String
--showAtom env (Con cid es) = env ^. constrNames . at' cid ++ bracket (map (showAtom env) es)
--showAtom _ e = "Can't show non constructor value: " ++ show e

bracket :: [String] -> String
bracket = foldr (\x s -> " (" ++ x ++ ")" ++ s) ""

showAtom :: Env Expr -> Expr -> String
showAtom env (Con cid es) = env ^. constrNames . at' cid ++ bracket (map (showAtom env) es)
showAtom _ e = "Can't show non constructor value: " ++ show e

printXVar :: Env Expr -> XId ->  String
printXVar env x = case env ^. free . at x of
  Just (cid, xs) -> env ^. constrNames . at' cid ++ bracket (map (printXVar env) xs)
  Nothing -> "_"

printXVar1 :: Env Expr -> XId ->  String
printXVar1 env x = case env ^. free . at x of
  Just (cid, xs) -> env ^. constrNames . at' cid ++ (concatMap (\a -> " " ++ show a)  xs)
  Nothing -> "_"

printXVars1 :: [Int] -> Env Expr -> String
printXVars1 xs env = concatMap (\x -> show x ++ " -> " ++ printXVar1 env x ++ "\n") xs 

printXVars :: [Int] -> Env Expr -> String
printXVars xs env = concatMap (\x -> " " ++ printXVar env x) xs

getFreeExpr :: Env Expr -> XId -> Expr
getFreeExpr env x = case env ^. free . at x of
  Just (cid, xs) -> Con cid (map (getFreeExpr env) xs)
  Nothing -> Bottom

printNeatExpr :: Env Expr -> Expr -> String
printNeatExpr env (Con cid es) = env ^. constrNames . at' cid ++ bracket (map (printNeatExpr env) es)
printNeatExpr _ Bottom = "_"
printNeatExpr _ _ = error "should only print constructors or bottom"
