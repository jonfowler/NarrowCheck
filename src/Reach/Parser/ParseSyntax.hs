{-# LANGUAGE DeriveDataTypeable #-}

module Reach.Parser.ParseSyntax where

import Data.Generics.Uniplate.Data
import Data.Data

data Def vid vid = Def vid [vid] Exp
  deriving (Show, Eq, Data, Typeable)

data Alt vid = Alt Pattern Exp 
  deriving (Show, Eq, Data, Typeable)

data Pattern vid = ConP vid [Pattern]
             | VarP vid
  deriving (Show, Eq, Data, Typeable)

data Exp vid 
  = Ap Exp [Exp]
  | Con vid [Exp]
  | Var vid
  | Lam [vid] Exp
  | Case Exp [Alt]
  | Target
  deriving (Show, Eq, Data, Typeable)


