{-# LANGUAGE DeriveDataTypeable #-}

module Reach.Parser.ParseSyntax where

import Data.Generics.Uniplate.Data
import Data.Data

data Def = Def VarID [VarID] Exp
  deriving (Show, Eq, Data, Typeable)


type VarID = String

data Alt = Alt Pattern Exp 
  deriving (Show, Eq, Data, Typeable)

data Pattern = ConP VarID [Pattern]
             | VarP VarID
  deriving (Show, Eq, Data, Typeable)

data Exp 
  = Ap Exp [Exp]
  | Con VarID [Exp]
  | Var VarID
  | Lam [VarID] Exp
  | Case Exp [Alt]
  | Target
  deriving (Show, Eq, Data, Typeable)


