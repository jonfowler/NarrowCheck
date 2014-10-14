{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Reach.Syntax where

import Data.Data

newtype VarID = VarID {fromVarID :: Int } 
  deriving (Show, Eq, Num, Data, Typeable)

data ConID = ConID Int String
  deriving (Show, Data, Typeable)

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

data Func = Func
  { body :: Exp,
    fid :: FunID,
    name :: String,
    args :: [VarID],
    varNum :: VarID  
  }
  deriving (Show, Data, Typeable)
 


