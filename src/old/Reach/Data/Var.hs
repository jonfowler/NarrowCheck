module Reach.Data.Var where

class Eq a => Var a where
  nextV :: a -> a
  addToV :: Int  -> a -> a



instance Var Int where
  nextV = (+1)
  addToV = (+)


