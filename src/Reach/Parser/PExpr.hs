module PExpr where

data PExpr
  = PCase PExpr [PAlt PExpr]
  | PApp PExpr PExpr
  | PParens PExpr
  | PCon ConId []
  | POp PExpr OpId PExpr 
  | 
     
data PAlt a = PAlt

data Expr = Case Expr [Alt]
          | App Expr Expr
          | Parens Expr
          | ConE ConId [Expr]
          | ConHole
          | Op Expr OpId Expr
          | Var VarId deriving (Show)

