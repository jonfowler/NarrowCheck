{-# LANGUAGE DeriveFunctor #-}
module Reach.Eval.Expr where

--import Reach.Lens

import Control.Lens

import Control.Monad

type LId = Int 
type EId = Int
type CId = Int 
type FuncId = Int
type FId = Int


data Alt a = Alt !CId [LId] a deriving (Show, Functor)

data Conts = [Alt Cont] :<: Conts
           | Expr :$: Conts 
           | Fin deriving Show

contsMap :: (Cont -> Cont) -> (Expr -> Expr) -> Conts -> Conts
contsMap f g Fin = Fin
contsMap f g (as :<: cs) = fmap (fmap f) as :<: contsMap f g cs
contsMap f g (e :$: cs) = g e :$: contsMap f g cs

(+++) :: Conts -> Conts -> Conts
Fin +++ c = c
as :<: cs +++ c = as :<: cs +++ c
e :$: cs +++ c = e :$: cs +++ c


data Cont = Cont !Expr !Conts deriving Show

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
  | Case Expr [Alt Expr] 

  -- A constructors arguments should be atoms: either a variable or
  -- further atoms. This is for efficiency, ensuring every expression
  -- is only evaluated once.
  | Con !CId [Atom] deriving Show


toCont :: Expr -> Cont
toCont e = Cont e Fin


-- Atoms are nested constructors with variables at their leaves.
type Atom = Expr

data Func =
  Func {_body :: Expr,
        _vars :: Int
       } deriving Show

makeLenses ''Func



