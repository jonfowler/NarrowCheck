{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Reach.Eval.Expr where

import Control.Lens

type LId = Int 
type CId = Int 
type FId = Int

data Expr
  = Let !LId Expr Expr
  | Fun {-# UNPACK #-} !FId
  | Var !LId
  | App Expr Atom
  | Lam !LId Expr
  | Case Expr [Alt]
  | Con !CId [Atom]

data Alt = Alt !CId [LId] Expr

type Atom = Expr

data Func =
  Func {_body :: Expr,
        _vars :: Int
       }

makeLenses ''Func

-- An atom should either be a Var or (Con [Atom])


