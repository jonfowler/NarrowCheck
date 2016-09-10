module Tree where

import Prelude ()
import ReachPrelude

data Tree = Leaf | Node Tree Nat Tree

{-# DIST Leaf 1 #-}
{-# DIST Node 2 #-}

reach :: Nat -> Tree -> Bool
reach n t = ord t && (depth t < s8)

allT p Leaf = True
allT p (Node t1 x t2) = p x && allT p t1 && allT p t2

allle i Leaf = True 
allle i (Node t1 x t2) = x <= i && allle i t1 && allle i t2

allge i Leaf = True
allge i (Node t1 x t2) = x >= i && allge i t1 && allge i t2

del n Leaf = Leaf
del n (Node t1 a t2) = if' (a < n)
    (Node t1 a (del n t2))
    (if' (n > a)
      (Node (del n t1) a t2)
      (ext t1 t2))

ext Leaf t2 = t2
ext (Node t11 a t12) t2 = Node t11 a (ext t12 t2)

ord Leaf = True
ord (Node t1 a t2) = allle a t1 && allge a t2

depth Leaf = Z 
depth (Node t1 x t2) = S (max (depth t1) (depth t2))

--prop_ordDel n t = ord t ==> ord (del n t)


