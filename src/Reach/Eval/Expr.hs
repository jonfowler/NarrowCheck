module Reach.Eval.Expr where

--import Reach.Lens

import Control.Lens

import Control.Monad

type LId = Int 
type EId = Int
type CId = Int 
type FuncId = Int
type FId = Int


data Alt a = Alt !CId [LId] a deriving (Show, Functor, Foldable, Traversable)

data Conts = Branch [Alt Expr]
           | Apply Expr deriving (Show)

data Expr = Expr Atom [Conts]
  | Let !LId Expr Expr
          deriving Show

data Atom 
  = Fun {-# UNPACK #-} !FuncId

  -- Environment variables are the variables used to implement
  -- call by need evaluation.
  | EVar !EId

  -- The "local" variables are variables scoped by lambdas, they
  -- are converted to environment variables when they are bound.
  | LVar !LId

  -- FVar's are the free variables
  | FVar !FId
--  | App Expr Expr 
  | Lam !LId Expr
--  | Case Expr [Alt Expr] 

  -- A constructors arguments should be atoms: either a variable or
  -- further atoms. This is for efficiency, ensuring every expression
  -- is only evaluated once.
  | Con !CId [Atom] deriving Show


closedExpr :: Expr -> Bool
closedExpr = closedExpr' []

closedExpr' :: [LId] -> Expr -> Bool 
closedExpr' vs (Let x e e') = closedExpr' (x : vs) e && closedExpr' (x : vs) e'
closedExpr' vs (Expr a cs) = closedAtom vs a && all (closedConts vs) cs

closedAtom :: [LId] -> Atom -> Bool
closedAtom vs (Fun _) = True
closedAtom vs (EVar _) = True
closedAtom vs (LVar v) = v `elem` vs
closedAtom vs (FVar _) = True
closedAtom vs (Lam v e) = closedExpr' (v : vs) e
closedAtom vs (Con _ as) = all (closedAtom vs) as

closedConts :: [LId] -> Conts -> Bool
closedConts vs (Apply e) = closedExpr' vs e
closedConts vs (Branch as) = all closedAlt as
  where closedAlt (Alt _ vs' e) = closedExpr' (vs' ++ vs) e

atom :: Atom -> Expr
atom a = Expr a []
--toCont :: Expr -> Cont
--toCont e = Cont e [] 

--toExpr :: Cont -> Expr
--toExpr (Cont e []) = e
--toExpr (Cont e (Apply e' : cs)) = toExpr (Cont (App e e') cs)
--toExpr (Cont e (Branch as : cs)) = toExpr (Cont (Case e (fmap (fmap toExpr) as)) cs)


-- Atoms are nested constructors with variables at their leaves.

data Func =
  Func {_body :: Expr,
        _vars :: !Int
       } deriving Show

makeLenses ''Func



