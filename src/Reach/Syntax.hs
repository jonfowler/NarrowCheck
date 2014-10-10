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
  = Ap FunID [Exp]
  | Con ConID [Exp]
  | Var VarID
  | Lam VarID Exp
  | Case Exp [Alt]
  deriving (Show, Data, Typeable)

data Fun = Fun
  { body :: Exp,
    fid :: FunID,
    name :: String,
    args :: [VarID],
    varNum :: Int  
  }
  deriving (Show, Data, Typeable)
 


