module TreeTrad where

import Prelude ()
import OverlapPrelude

data Tree = Leaf | Node Tree Nat Tree

{-# DIST Leaf 1 #-}
{-# DIST Node 5 #-}

checkn :: Nat -> Nat -> Tree -> Result 
checkn i n t = (ordered t *&&* (depth t <= i))
                                      ==> ordered (del n t) 

check :: Nat -> Tree -> Result 
check n t = checkn s5 n t 


allle i Leaf = True 
allle i (Node t1 x t2) = x <= i *&&* allle i t1 *&&* allle i t2

allge i Leaf = True
allge i (Node t1 x t2) = x >= i *&&* allge i t1 *&&* allge i t2

del n Leaf = Leaf
del n (Node t1 a t2) = if' (a < n)
    (Node t1 a (del n t2))
    (if' (n > a)
      (Node (del n t1) a t2)
      (ext t1 t2))

ext Leaf t2 = t2
ext (Node t11 a t12) t2 = Node t11 a (ext t12 t2)

ordered Leaf = True
ordered (Node t1 a t2) =  allle a t1 *&&*
                          allge a t2 *&&*
                          ordered t1 *&&*
                          ordered t2

depth Leaf = Z 
depth (Node t1 x t2) = S (maxTrad (depth t1) (depth t2))

--prop_ordDel n t = ord t ==> ord (del n t)


