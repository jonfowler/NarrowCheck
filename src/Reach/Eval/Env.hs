module Reach.Eval.Env (
  Env(..),
  Expr(..),
  Atom(..),
  FullAlts,
  Alts,
  atom,
  toExpr,
  funcs, funcArgTypes,
  free, nextFVar, freeDepth, freeType, maxDepth, topFrees,
  env, nextEVar, nextLVar, typeConstr,
  funcNames, funcIds, constrNames, constrIds,
  showAtom, printFVar, printFVar1)
  where

import Reach.Eval.ExprBase
import qualified Reach.Eval.Expr as E

import qualified Data.IntMap as I
import Data.IntMap (IntMap)
import qualified Data.Map as M
import Data.Map (Map)
import Reach.Lens

type Alts = [Alt Expr]
type FullAlts = [(Expr, [Alts])]
--type Expr' = (Atom, [Expr], FullAlts)

data Expr = Expr !Atom ![Expr] !FullAlts
          | Let LId Expr Expr deriving (Show, Eq)

data Atom = Fun {-# UNPACK #-} !FuncId
  -- The "local" variables are variables scoped by lambdas, they
  -- are converted to environment variables when they are bound.
  | Var {-# UNPACK #-} !LId
  -- FVar's are the free variables
  | FVar {-# UNPACK #-} !FId

  | Lam {-# UNPACK #-} !LId Expr

  | Bottom
  -- A constructors arguments should be atoms: either a variable or
  -- further atoms. This is for efficiency, ensuring every expression
  -- is only evaluated once.
  | Con {-# UNPACK #-} !CId [Atom] deriving (Show, Eq)

toExpr :: [Expr] -> FullAlts -> E.Expr ->  Expr 
toExpr ap br (E.App e e') = toExpr (toExpr [] [] e' : ap) br e
toExpr [] ((z , ass) : br) (E.Case e E.Bottom as) = toExpr  [] ((z , as' :ass) : br) e
   where as' = fmap (toExpr [] []) <$> as
toExpr [] br (E.Case e z as) = toExpr  [] ((toExpr [] [] z, [fmap (toExpr [] []) <$> as]) : br) e
toExpr ap br (E.Let x e e') = Let x (toExpr [] [] e) (toExpr ap br e')
toExpr ap br a = Expr (toAtom a) ap br

toAtom :: E.Expr -> Atom
toAtom (E.Fun f) = Fun f 
toAtom (E.Var v) = Var v
toAtom (E.FVar x) = FVar x
toAtom E.Bottom = Bottom 
toAtom (E.Con cid es) = Con cid (toAtom <$> es)
toAtom (E.Lam v e) = Lam v (toExpr [] [] e)

atom :: Atom -> Expr
atom a = Expr a [] []


data Env a = Env {
  _funcs :: IntMap (Func a),
  _funcArgTypes :: IntMap [Type],

  _free :: IntMap (CId, [FId]),
  _nextFVar :: !FId,
  _freeDepth :: IntMap Int,
  _freeType :: IntMap Type,

  _maxDepth :: !Int,
  _topFrees :: [FId],

  _env :: IntMap a,
  _nextEVar :: !EId,
  _nextLVar :: !LId,

  _typeConstr :: IntMap [(CId, [Type])],


  _funcNames :: IntMap String,
  _funcIds :: Map String FuncId,
  _constrNames :: IntMap String,
  _constrIds :: Map String CId
  } deriving Functor

makeLenses ''Env

showAtom :: Env Expr -> Atom -> String
showAtom env (Con cid es) = env ^. constrNames . at' cid ++ bracket (map (showAtom env) es)
showAtom _ e = "Can't show non constructor value: " ++ show e

bracket :: [String] -> String
bracket = foldr (\x s -> " (" ++ x ++ ")" ++ s) ""


printFVar :: Env Expr -> FId ->  String
printFVar env x = case env ^. free . at x of
  Just (cid, xs) -> env ^. constrNames . at' cid ++ bracket (map (printFVar env) xs)
  Nothing -> "_"

printFVar1 :: Env Expr -> FId ->  String
printFVar1 env x = case env ^. free . at x of
  Just (cid, xs) -> env ^. constrNames . at' cid ++ (concatMap (\a -> " " ++ show a)  xs)
  Nothing -> "_"
