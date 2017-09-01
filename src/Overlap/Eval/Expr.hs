module Overlap.Eval.Expr where

import qualified Data.IntMap as I
import GHC.Generics
import Control.DeepSeq
import Overlap.Lens
import Debug.Trace

type LId = Int
type CId = Int
type FId = Int
type XId = Int

type TId = Int

data Type = Type TId [Type] deriving (Generic)

data TypeExpr = TVar Int | TGlob Int | TApp TypeExpr TypeExpr deriving (Generic)


applyType :: [Type] -> TypeExpr ->  Type
applyType _ (TVar (-1)) = Type (-1) []
applyType ts (TVar i) = trace (show i) $ ts !! i
applyType _ (TGlob t) = Type t []
applyType ts (TApp x y) = let (Type t xs) = applyType ts x in Type t (xs ++ [applyType ts y])

data NarrowSet = Narrow {_getNarrowSet :: [(CId,Int,[NarrowSet])]} deriving (Generic)

makeLenses ''NarrowSet

data Expr
  = Let !LId Expr Expr
  | App Expr [Expr]
  | Fun !FId
  | Var !LId
  | FVar !XId
  | Lam !LId Expr
  | Bottom
  | Con !CId [Atom]
  | Local !FId (I.IntMap Expr) Def deriving (Show,Generic)

type Atom = Expr

data Def = Match Int (I.IntMap ()) [Alt] (I.IntMap ()) !(Maybe Def)
         | Result [Int] Expr deriving (Show,Generic)

data Alt = Alt CId [Int] Def
         | AltDef Def deriving (Show,Generic)

instance NFData Expr
instance NFData Alt
instance NFData Def
instance NFData TypeExpr
instance NFData Type
instance NFData NarrowSet

atom :: Expr -> Bool
atom (Var _) = True
atom (FVar _) = True
atom (Lam _ _) = error "lambda should not be tested whether atom is"
atom (Fun _) = True
atom (Con _ _) = True
atom Bottom = True
atom _ = False

