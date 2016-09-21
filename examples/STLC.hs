module STLC where

import Prelude ()
import OverlapPrelude

data Expr = Add Expr Expr
          | If Expr Expr Expr
          | N Nat 
          | B Bool 
          | Var Nat
            -- The app constructor comes with the type of its argument
            -- this can be thrown away after
          | App Type Expr Expr
          | Lam Expr
            
data Type = TypeN | TypeB | Fun Type Type | NoType

data Eval = Val Expr | Error

TypeN =|= TypeN = True
TypeB =|= TypeB = True
Fun t t' =|= Fun u u' = (t =|= u) && (t' =|= u')
a =|= b = False

data TList = C Type TList | E

closed' :: Expr -> Nat -> Bool
closed' (Add e e') n = closed' e n && closed' e' n
closed' (If e e' e'') n = closed' e n && closed' e' n && closed' e'' n 
closed' (App t e e') n = closed' e n && closed' e' n
closed' (Lam e) n = closed' e (S n) 
closed' (Var v) n = v <= n
closed' m n = True

closed :: Expr -> Bool
closed e = closed' e Z 

oftype :: Expr -> Type -> TList ->  Bool
oftype (Add e e') TypeN ts = oftype e TypeN ts && oftype e' TypeN ts
oftype (If e e' e'') t ts = oftype e t ts && oftype e' t ts && oftype e'' t ts
oftype (N n) TypeN ts = True 
oftype (B b) TypeB ts = True
oftype (Var v) t ts = oftypeVar v t ts
oftype (Lam e) (Fun t t') ts = oftype e t' (C t ts)
oftype (App t' e e') t ts = oftype e (Fun t' t) ts && oftype e' t' ts
oftype e t c = False

oftypeVar Z t (C t' ts) = t =|= t'
oftypeVar (S x) t (C t' ts) = oftypeVar x t ts
oftypeVar a b c = False

depthExpr :: Expr -> Nat
depthExpr (Add e e') = S (max (depthExpr e) (depthExpr e'))
depthExpr (If e e' e'') = S (max (depthExpr e)
                            (max (depthExpr e')
                                 (depthExpr e'')))
depthExpr (App t' e e') = S (max (depthExpr e) (depthExpr e'))
depthExpr (Lam e) = depthExpr e
depthExpr x = Z

subst :: Nat -> Expr -> Expr -> Expr
subst n e (Var v) = if (v < n)
                       (Var v)
                       (if (v == n)
                           (  ))
subst n e (Lam e') = subst (S n) e e'

check :: Expr -> Type -> Result
check e t = oftype e t E && (depthExpr e <= s5)
                 ==> True

{-# DIST Add 3 #-}
{-# DIST If 3 #-}

