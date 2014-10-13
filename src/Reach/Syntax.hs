{-# LANGUAGE DeriveDataTypeable #-}

module Reach.Syntax where

import Data.Data

type VarID = Int 

data ConID = ConID Int String
  deriving (Show, Data, Typeable)

type FunID = Int

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
    varNum :: Int  
  }
  deriving (Show, Data, Typeable)
 


