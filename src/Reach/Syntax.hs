{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Reach.Syntax where

import Data.Data

newtype VarID = VarID {fromVarID :: Int } 
  deriving (Show, Eq, Num, Data, Typeable)

data ConID = ConID Int String
  deriving (Show, Data, Typeable)

instance Eq ConID where
  ConID a _ == ConID b _ = a == b

newtype FunID = FunID {fromFunID :: Int}
  deriving (Show, Eq, Num, Data, Typeable)

data Alt = Alt ConID [VarID] Exp 
  deriving (Show, Data, Typeable)

data Exp 
  = Ap Exp [Exp]
  | Con ConID [Exp]
  | Var VarID
  | Fun FunID
  | Lam VarID Exp
  | Case Exp [Alt]
  | Target
  deriving (Show, Data, Typeable)

data CaseS
  = CaseS ExpS [AltS]
  deriving (Show, Data, Typeable)

data AltS = AltS
  deriving (Show, Data, Typeable)

data ExpS 
  = ApS CaseS [CaseS]
  | ConS ConID [CaseS]
  | VarS VarID
  | FunS FunID
  | LamS VarID Exp
  | TargetS
  deriving (Show, Data, Typeable)

data Func = Func
  { body :: Exp,
    fid :: FunID,
    name :: String,
    args :: [VarID],
    varNum :: VarID  
  }
  deriving (Show, Data, Typeable)
 


