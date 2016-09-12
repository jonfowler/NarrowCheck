module Tree where

import Prelude ()
import ReachPrelude

data Tree = Leaf | Node Tree Nat Tree

{-# DIST Leaf 1 #-}
{-# DIST Node 5 #-}

reach :: Nat -> Tree -> Result 
reach n t = (ordered t && (depth t <= s8)) ==> True

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

-- Incorrect definiton of ordering 
ord Leaf = True
ord (Node t1 a t2) = allle a t1 && allge a t2

-- Correct defintion of ordering                    

ordered Leaf = True
ordered (Node t1 a t2) = allle a t1 && allge a t2 && ordered t1 && ordered t2


depth Leaf = Z 
depth (Node t1 x t2) = S (max (depth t1) (depth t2))

depthBad Leaf = Z                       
depthBad (Node t1 x t2) = max x (S (max (depth t1) (depth t2)))

--prop_ordDel n t = ord t ==> ord (del n t)


