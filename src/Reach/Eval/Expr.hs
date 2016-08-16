module Reach.Eval.Expr where

import qualified Data.IntMap as I

type LId = Int 
type CId = Int 
type FId = Int
type XId = Int
type Type = Int

data Expr
  = Let !LId Expr Expr
  | App Expr [Expr]
  | Fun !FId
  | Var !LId
  | FVar !XId
  | Lam !LId Expr
  | Bottom
  | Con !CId [Atom]
  | Local (I.IntMap Expr) Def deriving (Show)

type Atom = Expr

data Def = Match Int [Alt] !(Maybe Def)
         | Result [Int] Expr deriving (Show)

data Alt = Alt CId [Int] Def
         | AltDef Def deriving (Show)

atom :: Expr -> Bool            
atom (Var _) = True
atom (FVar _) = True
atom (Lam _ _) = error "lambda should not be tested whether atom is"
atom (Fun _) = True
atom (Con _ _) = True
atom Bottom = True
atom _ = False

