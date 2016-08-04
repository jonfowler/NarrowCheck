module Reach.New.Expr where

import qualified Data.IntMap as I

type LId = Int 
type CId = Int 
type FId = Int
type XId = Int
type Type = Int

data Expr
  = Let !LId Expr Expr
  | App Expr Expr
  | Fun !FId
  | Var !LId
  | FVar !XId
  | Lam !LId Expr
  | Bottom
  | Con !CId [Atom]
  | Local (I.IntMap Expr) Def

type Atom = Expr

data Def = Match [(Int, [Alt])]
         | Result ([Int], Expr)

data Alt = Alt CId [Int] Def
         | AltDef Def
