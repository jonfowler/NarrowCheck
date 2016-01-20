module Reach.Parser.PExpr where

import Reach.Lens

type OpId = String
type ConId = String
type VarId = String
type TypeId = String

data PExpr
  -- The following constructors are the "core" constructors, they are used for
  -- parsing and then PExpr are converted to only use these types
  = PCase PExpr [PAlt PExpr]
  | PApp PExpr PExpr
  | PCon ConId [PExpr]
  | PVar VarId
  | PLam VarId PExpr
  | PLet VarId PExpr PExpr

  -- The following are used in parsing but then desugared to the above constructors
  | PParens PExpr
  | POp PExpr OpId PExpr 
  | POpVar OpId
  | POpL PExpr OpId
  | POpR OpId PExpr
  deriving (Show)
     
data PAlt a = PAlt Pattern a deriving (Show)

data Pattern = PatVar VarId
             | PatCon ConId [Pattern] deriving (Show)

data PDef = PDef {_defName :: VarId, _defArgs :: [Pattern], _defBody :: PExpr} deriving (Show)
makeLenses ''PDef

data Type = Type :-> Type
          | Type TypeId deriving (Show)

data PData = PData {_dataName :: TypeId, _dataCon :: [(ConId, [Type])]} deriving (Show)
makeLenses ''PData



--data Expr = Case Expr [Alt]
--          | App Expr Expr
--          | Parens Expr
--          | ConE ConId [Expr]
--          | ConHole
--          | Op Expr OpId Expr
--          | Var VarId deriving (Show)

