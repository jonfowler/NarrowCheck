{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Reach.Parser.Parse where

newtype TypeID = Tid String deriving (Eq, Ord)
newtype ConstID = Cid String deriving (Eq, Ord)

data Const = Const ConstID [TypeID]
data Type = Type TypeID [Const]

