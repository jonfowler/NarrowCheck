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

welltyped :: Type -> Bool
welltyped NoType = False
welltyped x = True 

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

oftype and (N u) TypeN = True
oftype and (B v) TypeB = True
oftype and (If e e' e'') t =  and (oftype and e TypeB)
                             (and (oftype and e' t)
                                  (oftype and e'' t))
oftype and (Add e e') TypeN =  and (oftype and e TypeN)
                                   (oftype and e' TypeN)
oftype and u v = False

check :: Type -> Expr -> Result 
check t e = (oftype (andTrad) e t && (depthExpr e <= s7)) ==> noError (evalExpr e)

--check :: Expr -> Result
--check e = (welltyped (typeof e) && (depthExpr e <= s7)) ==> noError (evalExpr e) 
            
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

