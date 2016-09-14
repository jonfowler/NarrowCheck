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

oftypeVar Z t (C t' ts) = t =|= t'
oftypeVar (S x) t (C t' ts) = oftypeVar x t ts
oftypeVar a b c = False

{-# DIST Add 3 #-}
{-# DIST If 3 #-}

