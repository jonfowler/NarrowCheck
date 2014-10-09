{-# LANGUAGE DeriveDataTypeable #-}

module Reach.Parser.ParseSyntax where

import Data.Generics.Uniplate.Data
import Data.Data

data Def = Def VarID [VarID] Exp
  deriving (Show, Eq, Data, Typeable)


type VarID = String

data Alt = Alt VarID [VarID] Exp 
  deriving (Show, Eq, Data, Typeable)

data Exp 
  = Ap Exp [Exp]
  | Con VarID [Exp]
  | Var VarID
  | Lam [VarID] Exp
  | Case Exp [Alt]
  deriving (Show, Eq, Data, Typeable)


