module Syntax where

data VarID = VarID Int String

data Alt = Alt VarID [VarID] Exp 

data Exp 
  = Ap Exp Exp
  | Var VarID
  | Lam VarID Exp
  | Case Exp [Alt]


