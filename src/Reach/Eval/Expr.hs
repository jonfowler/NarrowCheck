module Reach.Eval.Expr where

--import Reach.Lens

import Control.Lens

import Control.Monad

import Data.IntMap (IntMap)       
import qualified Data.IntMap as I

type LId = Int 
type EId = Int
type CId = Int 
type FuncId = Int
type FId = Int
type Type = Int


data Alt a = Alt {-# UNPACK #-} !CId [LId] a
           | AltDef a deriving (Show, Functor, Foldable, Traversable)

altExpr :: Alt a -> a                               
altExpr (Alt _ _ e) = e
altExpr (AltDef e) = e

--data Conts = Branch Expr [Alt Expr]
--           | Apply Expr deriving (Show)

data Expr
  = Let !LId Expr Expr
  | Fun {-# UNPACK #-} !FuncId
  -- The "local" variables are variables scoped by lambdas, they
  -- are converted to environment variables when they are bound.
  | Var {-# UNPACK #-} !LId
  -- FVar's are the free variables
  | FVar {-# UNPACK #-} !FId

  | Lam {-# UNPACK #-} !LId Expr

  | Bottom
  | Case Expr Expr [Alt Expr] 
  | App Expr Expr
  -- A constructors arguments should be atoms: either a variable or
  -- further atoms. This is for efficiency, ensuring every expression
  -- is only evaluated once.
  | Con {-# UNPACK #-} !CId [Atom] deriving Show

type Atom = Expr
                            
--closedExpr :: Expr -> Bool
--closedExpr = closedExpr' []
--
--closedExpr' :: [LId] -> Expr -> Bool 
--closedExpr' vs (Let x e e') = closedExpr' (x : vs) e && closedExpr' (x : vs) e'
--closedExpr' vs (Expr a cs) = closedAtom vs a && all (closedConts vs) cs
--
--closedAtom :: [LId] -> Atom -> Bool
--closedAtom vs (Fun _) = True
--closedAtom vs (Var v) = v `elem` vs
--closedAtom vs (FVar _) = True
--closedAtom vs (Lam v e) = closedExpr' (v : vs) e
--closedAtom vs (Con _ as) = all (closedAtom vs) as
--
--closedConts :: [LId] -> Conts -> Bool
--closedConts vs (Apply e) = closedExpr' vs e
--closedConts vs (Branch a as) = closedExpr' vs a && all closedAlt as
--  where closedAlt (Alt _ vs' e) = closedExpr' (vs' ++ vs) e
--
--atom :: Atom -> Expr
--atom a = Expr a []
--toCont :: Expr -> Cont
--toCont e = Cont e [] 

--toExpr :: Cont -> Expr
--toExpr (Cont e []) = e
--toExpr (Cont e (Apply e' : cs)) = toExpr (Cont (App e e') cs)
--toExpr (Cont e (Branch as : cs)) = toExpr (Cont (Case e (fmap (fmap toExpr) as)) cs)


-- Atoms are nested constructors with variables at their leaves.

--lazify :: Expr -> Expr
--lazify (Let x e e')  = Let x (lazify e) (lazify e') 
--lazify (Expr e cs) = Expr (lazifyAtom e) (map lazifyCont cs)
--
--lazifyAtom :: Atom -> Atom                     
--lazifyAtom = undefined
--
--lazifyCont :: Conts -> Conts
--lazifyCont (Apply e) = Apply (lazify e)
--lazifyCont (Branch _ as) = Branch (unifyList (map altExpr as)) as
--
--unifyList :: [Expr] -> Expr                            
--unifyList [e] = e
--unifyList (e : es) = unify e (unifyList es)
--
--unify :: Expr -> Expr -> Expr
--unify _ (Let x e e') = Expr Bottom []
--unify e (Expr a cs) = unify' e a cs
--
--unify' :: Expr -> Atom -> [Conts] -> Expr
--unify' e a [] = unifyAtom e a
         

data Func =
  Func {_body :: Expr,
        _vars :: !Int
       } -- deriving Show

makeLenses ''Func



