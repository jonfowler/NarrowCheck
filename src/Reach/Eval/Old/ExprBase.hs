module Reach.Eval.ExprBase where

import Reach.Lens

type LId = Int 
type EId = Int
type CId = Int 
type FuncId = Int
type FId = Int
type Type = Int


data Alt a = Alt {-# UNPACK #-} !CId [LId] a
          | AltDef a deriving (Show, Functor, Foldable, Traversable, Eq)


altExpr :: Alt a -> a                               
altExpr (Alt _ _ e) = e
altExpr (AltDef e) = e

data Func a =
  Func {_body :: a,
        _vars :: !Int
       } deriving Functor
makeLenses ''Func

