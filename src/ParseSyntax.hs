module ParseSyntax where


data Def = Def VarID [VarID] Exp

data VarID = VarID Int String

data Alt = Alt VarID [VarID] Exp 

data Exp 
  = Ap Exp [Exp]
  | Brack Exp
  | Var VarID
  | Lam VarID Exp
  | Case Exp [Alt]

 
