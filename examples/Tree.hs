module Tree where

import Prelude ()
import OverlapPrelude

data Tree = Leaf | Node Tree Nat Tree

{-# DIST Leaf 1 #-}
{-# DIST Node 5 #-}

check :: Nat -> Tree -> Result 
check n t = (ord t && (depth t <= s7))
                                      ==> ord (del n t) 

checkTrad :: Nat -> Tree -> Result 
checkTrad n t = andTrad (depth t <= s4) (ordTrad t)
                                      ==> ordTrad (del n t)

ord = ordered (&&)
ordTrad = ordered andTrad

allT and p Leaf = True
allT and p (Node t1 x t2) =  and (p x)
                            (and (allT and p t1)
                                 (allT and p t2))

--
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

-- Correct defintion of ordering                    

le x y = y <= x
ge x y = y >= x

ordered and Leaf = True
ordered and (Node t1 a t2) =  and (allT and (le a) t1)
                             (and (allT and (ge a) t2)
                             (and (ordered and t1)
                                  (ordered and t2)))

depth Leaf = Z 
depth (Node t1 x t2) = S (max (depth t1) (depth t2))

depthBad Leaf = Z                       
depthBad (Node t1 x t2) = max x (S (max (depth t1) (depth t2)))

--prop_ordDel n t = ord t ==> ord (del n t)


