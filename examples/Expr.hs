module Expr where

import Prelude ()
import OverlapPrelude

data Expr = Add Expr Expr
          | If Expr Expr Expr
          | N Nat 
          | B Bool 

{-# DIST Add 3 #-}
{-# DIST If 3 #-}

data Type = TypeN | TypeB | NoType

depthExpr :: Expr -> Nat
depthExpr (N v) = Z
depthExpr (B v) = Z
depthExpr (If e e' e'') = S (max (depthExpr e) (max (depthExpr e') (depthExpr e'')))
depthExpr (Add e e') = S (max (depthExpr e) (depthExpr e'))

typeof :: Expr -> Type 
typeof (N v) = TypeN 
typeof (B v) = TypeB
typeof (If e e' e'') = typeIf (typeof e) (typeof e') (typeof e'') 
typeof (Add e e') = typeAdd (typeof e) (typeof e')

typeIf TypeB TypeN TypeN = TypeN                    
typeIf TypeB TypeB TypeB = TypeB                    
typeIf u v w = NoType

typeAdd TypeN TypeN = TypeN
typeAdd u v = NoType



oftype :: Expr -> Type -> Bool
oftype (N u) TypeN = True
oftype (B v) TypeB = True
oftype (If e e' e'') t = oftype e TypeB && oftype e' t && oftype e'' t
oftype (Add e e') TypeN = oftype e TypeN && oftype e' TypeN
oftype u v = False




reach :: Type -> Expr -> Result 
reach t e = (oftype e t && (depthExpr e <= s7)) ==> noError (evalExpr e)




            
data ExprRes = ResN Nat | ResB Bool | ResError

noError :: ExprRes -> Bool
noError ResError = False
noError u = True

evalExpr :: Expr -> ExprRes
evalExpr (N n) = ResN n
evalExpr (B b) = ResB b
--evalExpr (Add (N v) e') = ResB True
evalExpr (Add e e') = evalAdd (evalExpr e) (evalExpr e')
evalExpr (If e e' e'') = evalIf (evalExpr e) (evalExpr e') (evalExpr e'')

evalAdd (ResN n) (ResN m) = ResN (n + m)
evalAdd u v = ResError

evalIf (ResB True) p q = p
evalIf (ResB False) p q = q
evalIf u v w = ResError
