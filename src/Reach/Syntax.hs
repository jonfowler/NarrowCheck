module Reach.Syntax where

type VarID = Int 

data ConID = ConID Int String

type FunID = Int

data Alt = Alt ConID [VarID] Exp 

data Exp 
  = Ap FunID [Exp]
  | Con ConID [Exp]
  | Var VarID
  | Lam VarID Exp
  | Case Exp [Alt]

data Fun = Fun
  { body :: Exp,
    name :: FunID,
    args :: [VarID],
    varNum :: Int  
  }
 


