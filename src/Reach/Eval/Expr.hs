module Reach.Eval.Expr where

import Reach.Lens
import Data.DList

import Control.Monad

type LId = Int 
type EId = Int
type CId = Int 
type FuncId = Int
type FId = Int

data Expr
  = Let !LId Expr Expr
  | Fun {-# UNPACK #-} !FuncId

  -- Environment variables are the variables used to implement
  -- call by need evaluation.
  | EVar !EId

  -- The "local" variables are variables scoped by lambdas, they
  -- are converted to environment variables when they are bound.
  | LVar !LId

  -- FVar's are the free variables
  | FVar !FId
  | App Expr Expr 
  | Lam !LId Expr
  | Case Expr [Alt] 

  -- A constructors arguments should be atoms: either a variable or
  -- further atoms. This is for efficiency, ensuring every expression
  -- is only evaluated once.
  | Con !CId (DList Atom) deriving Show

data Alt = Alt !CId [LId] Expr deriving Show

-- Atoms are nested constructors with variables at their leaves.
type Atom = Expr

data Func =
  Func {_body :: Expr,
        _vars :: Int
       } deriving Show

makeLenses ''Func





-- An atom should either be a Var or (Con [Atom])


