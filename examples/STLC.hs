module STLC where

import Prelude (Show)
import OverlapPrelude

data Expr =  -- Add Expr Expr
             -- | If Expr Expr Expr
           -- |
           -- N Nat 
           B Bool
          | Var Nat
            -- The app constructor comes with the type of its argument
            -- this can be thrown away after
          | App Type Expr Expr
          | Lam Expr
          | Error deriving (Show)

-- {-# DIST If 2 #-}
-- {-# DIST Add 2 #-}
{-# DIST App 2 #-}
{-# DIST Lam 1 #-}
-- {-# DIST N 1 #-}
{-# DIST B 1 #-}
            
data Type = TypeN | TypeB | Fun Type Type | NoType deriving Show


TypeN =|= TypeN = True
TypeB =|= TypeB = True
Fun t t' =|= Fun u u' = (t =|= u) && (t' =|= u')
a =|= b = False

--closed' :: Expr -> Nat -> Bool
--closed' (Add e e') n = closed' e n && closed' e' n
--closed' (If e e' e'') n = closed' e n && closed' e' n && closed' e'' n 
--closed' (App t e e') n = closed' e n && closed' e' n
--closed' (Lam e) n = closed' e (S n) 
--closed' (Var v) n = v <= n
--closed' m n = True
--
--closed :: Expr -> Bool
--closed e = closed' e Z 

oftype :: Expr -> Type -> List Type ->  Bool
-- oftype (Add e e') TypeN ts = oftype e TypeN ts && oftype e' TypeN ts
-- oftype (If e e' e'') t ts = oftype e TypeB ts && oftype e' t ts && oftype e'' t ts
-- oftype (N n) TypeN ts = True 
oftype (B b) TypeB ts = True
oftype (Var v) t ts = oftypeVar v t ts
oftype (Lam e) (Fun t t') ts = oftype e t' (C t ts)
oftype (App t' e e') t ts = oftype e (Fun t' t) ts && oftype e' t' ts
oftype e t c = False

oftypeVar Z t (C t' ts) = t =|= t'
oftypeVar (S x) t (C t' ts) = oftypeVar x t ts
oftypeVar a b c = False

depthExpr :: Expr -> Nat
-- depthExpr (Add e e') = S (max (depthExpr e) (depthExpr e'))
-- depthExpr (If e e' e'') = S (max (depthExpr e)
--                            (max (depthExpr e')
--                                 (depthExpr e'')))
depthExpr (App t' e e') = S (max (depthExpr e) (depthExpr e'))
depthExpr (Lam e) = S (depthExpr e)
depthExpr (Var v) = v
--depthExpr (N n) = n
depthExpr x = Z

rename :: (Nat -> Nat) -> Expr -> Expr
--rename f (Add e e') = Add (rename f e) (rename f e')
--rename f (If e e' e'') = If (rename f e) (rename f e') (rename f e'')
rename f (App t' e e') = App t' (rename f e) (rename f e')
rename f (Lam e) = Lam (rename (extendRename f) e)
rename f (Var v) = Var (f v) 
rename f e = e

extendRename :: (Nat -> Nat) -> Nat -> Nat 
extendRename f Z = Z
extendRename f (S v) = S (f v)

extendSubst :: (Nat -> Expr) -> Nat -> Expr
extendSubst f Z = Var Z
extendSubst f (S v) = rename S (f v)

subst :: (Nat -> Expr) -> Expr -> Expr
-- subst f (Add e e') = Add (subst f e) (subst f e')
-- subst f (If e e' e'') = If (subst f e) (subst f e') (subst f e'')
subst f (App t' e e') = App t' (subst f e) (subst f e')
subst f (Lam e) = Lam (subst (extendSubst f) e)
subst f (Var v) = f v
subst f e = e

notFunc :: Type -> Bool
notFunc TypeN = True
notFunc TypeB = True
notFunc n = False

subZ :: Expr -> Expr -> Expr
subZ e e' = subst (repZ e) e'

repZ :: Expr -> Nat -> Expr 
repZ e Z = e
repZ e (S v) = Var v

eval :: Expr -> Expr
eval (App t e e') = evalApp (eval e) e'
-- eval (Add e e') = evalAdd (eval e) (eval e')
-- eval (If e e' e'') = evalIf (eval e) e' e'' 
eval e = e

noError :: Expr -> Bool
noError Error = False
noError e = True

evalApp (Lam e) e' = eval (subZ e' e)
evalApp e e' = Error

evalAdd :: Expr -> Expr -> Expr
-- evalAdd (N n) (N n') = N (n + n')
evalAdd e e' = Error

evalIf (B True) e e' = eval e
evalIf (B False) e e' = eval e'
evalIf e e' e'' = Error

check :: Expr -> Type -> Result
check e t = oftype e t E && notFunc t && (depthExpr e <= s2)
                 ==> noError (eval e)


