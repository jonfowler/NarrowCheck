module Reach.Eval.Expr where

--import Reach.Lens

import Control.Lens
import Data.Maybe

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
           | AltDef a deriving (Show, Functor, Foldable, Traversable, Eq)

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
  | Con {-# UNPACK #-} !CId [Atom] deriving (Show, Eq)
                         

type Atom = Expr

atom :: Expr -> Bool            
atom (Var _) = True
atom (FVar _) = True
atom (Lam _ _) = error "lambda should not be tested whether atom is"
atom (Fun _) = True
atom (Con _ _) = True
atom Bottom = True
atom _ = False

                            
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

lazify :: Expr -> Expr
lazify = lazify' []

lazify' :: [LId] -> Expr -> Expr
lazify' vs (Lam v e) = Lam v (lazify' (v : vs) e) 
lazify' vs (Let v e e') = Let v (lazify' vs e) (lazify' (v : vs) e')
lazify' vs (Fun fid) = Fun fid
lazify' vs (FVar x) = FVar x
lazify' vs (Var v) = Var v
lazify' vs (App e e') = App (lazify' vs e) (lazify' vs e')
lazify' vs (Con cid es) = Con cid (map (lazify' vs) es)
lazify' vs (Case e _ as) = Case (lazify e) (unifyList (map (delocalise vs . altExpr) as')) as'
    where as' = lazifyAlt <$> as 
          lazifyAlt (Alt cid vs' e) = Alt cid vs' (lazify' (vs' ++ vs) e)
          lazifyAlt (AltDef e) = AltDef (lazify' vs e)

--lazifyAtom :: Atom -> Atom                     
--lazifyAtom = undefined
--
--lazifyCont :: Conts -> Conts
--lazifyCont (Apply e) = Apply (lazify e)
--lazifyCont (Branch _ as) = Branch (unifyList (map altExpr as)) as
--
unifyList :: [Expr] -> Expr                            
unifyList [e] = e
unifyList (e : es) = unify e (unifyList es)

unify :: Expr -> Expr -> Expr
unify Bottom _ = Bottom
unify _ Bottom = Bottom
unify (Con cid []) (Con cid' [])
  | cid == cid' = Con cid []
  | otherwise = Bottom
unify (Con cid es) (Con cid' es')
--  | cid == cid' = error "just delete this for now"
  | otherwise = Bottom
unify (Let x e e') _ = Bottom
unify _ (Let x e e') = Bottom
unify (Var v) (Var v')
  | v == v' = Var v
  | otherwise = error "does different variables happen???" 
unify (Case e d as) e' = deCase e (unify d e') (fmap (unify e') <$> as)
unify e' (Case e d as) = deCase e (unify d e') (fmap (unify e') <$> as)
unify e (Con cid []) = Case e Bottom [Alt cid [] (Con cid [])]
unify (Con cid []) e = Case e Bottom [Alt cid [] (Con cid [])]
unify _ _ = Bottom

delocalise :: [LId] -> Expr -> Expr
delocalise vs (Let v e e') = Bottom
delocalise vs (Lam v e) = Lam v (delocalise (v : vs) e)
delocalise vs (Var v) | v `elem` vs = Var v
                      | otherwise = Bottom
delocalise vs (Fun f) = Fun f
delocalise vs (App e e') = deApp (delocalise vs e) (delocalise vs e')
delocalise vs (Con cid es) = deCon cid (map (delocalise vs) es)
delocalise vs (FVar x) = FVar x
delocalise vs Bottom = Bottom
delocalise vs (Case e e' as) = deCase (delocalise vs e) (delocalise vs e') (mapMaybe delocalAlt as)
   where                            
   delocalAlt (Alt cid vs' e) = case delocalise (vs'++vs) e of
                                Bottom -> Nothing
                                e' -> Just (Alt cid vs' e' )
   delocalAlt (AltDef e) = case delocalise vs e of
                                Bottom -> Nothing
                                e' -> Just (AltDef e')

deApp :: Expr -> Expr -> Expr
deApp Bottom _ = Bottom
deApp _ Bottom = Bottom
deApp e e' = App e e'

deCon :: CId -> [Expr] -> Expr
deCon cid es | all (/= Bottom) es = Con cid es
             | otherwise = Bottom

deCase :: Expr -> Expr -> [Alt Expr] -> Expr
deCase Bottom _ _ = Bottom
deCase e e' as = case filter ((/=Bottom) . altExpr) as of
  [] -> e'
  as' -> Case e e' as' 

data Func =
  Func {_body :: Expr,
        _vars :: !Int
       } -- deriving Show

makeLenses ''Func



