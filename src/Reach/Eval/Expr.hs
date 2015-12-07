module Reach.Eval.Expr where

import Control.Lens 
import Data.DList

import Control.Monad

type LId = Int 
type CId = Int 
type FId = Int

data Expr
  = Let !LId Expr Expr
  | Fun {-# UNPACK #-} !FId
  | Var !LId
  | App Expr Expr 
  | Lam !LId Expr
  | Case Expr [Alt]

  -- A constructors arguments should be atoms: either a variable or
  -- further atoms. This is for efficiency, ensuring every expression
  -- is only evaluated once.
  | Con !CId (DList Atom) 

data Alt = Alt !CId [LId] Expr

-- Atoms are nested constructors with variables at their leaves.
type Atom = Expr

data Func =
  Func {_body :: Expr,
        _vars :: Int
       }

makeLenses ''Func





-- An atom should either be a Var or (Con [Atom])


