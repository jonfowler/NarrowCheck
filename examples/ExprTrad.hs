module ExprTrad where

import Prelude ()
import OverlapPrelude



data Expr = Add Expr Expr
          | If Expr Expr Expr
          | Natural Nat 
          | Boolean Bool 

data Type = TypeN | TypeB | NoType


check :: Type -> Expr -> Result 
check t e = checkn s7 t e 

checkn :: Nat -> Type -> Expr -> Result
checkn n t e = sized (oftype e t ==> noError (evalExpr e)) (depthExpr e <= n)

enumcheckn :: Nat -> Type -> Expr -> Result
enumcheckn n t e = sized
  (oftype e t ==> noError (evalExpr e))
  (countExpr e <= n)

countExpr :: Expr -> Nat
countExpr (Natural v) = v
countExpr (Boolean v) = Z
countExpr (If e e' e'') = S (countExpr e + countExpr e' + countExpr e'')
countExpr (Add e e') = S (countExpr e + countExpr e')

depthExpr :: Expr -> Nat
depthExpr (Natural v) = v
depthExpr (Boolean v) = Z
depthExpr (If e e' e'') = S (max (depthExpr e) (max (depthExpr e') (depthExpr e'')))
depthExpr (Add e e') = S (max (depthExpr e) (depthExpr e'))

welltyped :: Type -> Bool
welltyped NoType = False
welltyped x = True 

typeof :: Expr -> Type 
typeof (Natural v) = TypeN 
typeof (Boolean v) = TypeB
typeof (If e e' e'') = typeIf (typeof e) (typeof e') (typeof e'') 
typeof (Add e e') = typeAdd (typeof e) (typeof e')

typeIf TypeB TypeN TypeN = TypeN                    
typeIf TypeB TypeB TypeB = TypeB                    
typeIf u v w = NoType

typeAdd TypeN TypeN = TypeN
typeAdd u v = NoType

oftype (Natural u) TypeN = True
oftype (Boolean v) TypeB = True
oftype (If e e' e'') t =  oftype e TypeB *&&* oftype e' t *&&* oftype e'' t
oftype (Add e e') TypeN =  oftype e TypeN *&&* oftype e' TypeN
oftype u v = False

--check :: Expr -> Result
--check e = (welltyped (typeof e) && (depthExpr e <= s7)) ==> noError (evalExpr e) 
            
data ExprRes = ResN Nat | ResB Bool | ResError

noError :: ExprRes -> Bool
noError ResError = False
noError u = True

evalExpr :: Expr -> ExprRes
evalExpr (Natural n) = ResN n
evalExpr (Boolean b) = ResB b
--evalExpr (Add (Natural v) e') = ResB True
evalExpr (Add e e') = evalAdd (evalExpr e) (evalExpr e')
evalExpr (If e e' e'') = evalIf (evalExpr e) (evalExpr e') (evalExpr e'')

evalAdd (ResN n) (ResN m) = ResN (n + m)
evalAdd u v = ResError

evalIf (ResB True) p q = p
evalIf (ResB False) p q = q
evalIf u v w = ResError

