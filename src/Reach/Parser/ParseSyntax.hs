module ParseSyntax where


data Def = Def VarID [VarID] Exp
  deriving (Show)

type VarID = String

data Alt = Alt VarID [VarID] Exp 
  deriving (Show)

data Exp 
  = Ap Exp [Exp]
  | Var VarID
  | Lam [VarID] Exp
  | Case Exp [Alt]
  deriving (Show)

 
