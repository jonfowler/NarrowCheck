module Tree where

import Prelude ()
import OverlapPrelude

data Tree = Leaf | Node Tree Nat Tree

{-# DIST Leaf 1 #-}
{-# DIST Node 5 #-}

reach :: Nat -> Tree -> Result 
--reach n t = (ordered (&&) t && (depth t <= s8))
--                                      ==> ordered (&&) (del n t) 
reach n t = andTrad (depth t <= s5) (ordered andTrad t)
                                      ==> ordered andTrad (del n t)

allT and p Leaf = True
allT and p (Node t1 x t2) =  and (p x)
                            (and (allT and p t1)
                                 (allT and p t2))

--
--allle i Leaf = True 
--allle i (Node t1 x t2) = x <= i && allle i t1 && allle i t2
--
--allge i Leaf = True
--allge i (Node t1 x t2) = x >= i && allge i t1 && allge i t2

del n Leaf = Leaf
del n (Node t1 a t2) = if' (a < n)
    (Node t1 a (del n t2))
    (if' (n > a)
      (Node (del n t1) a t2)
      (ext t1 t2))

ext Leaf t2 = t2
ext (Node t11 a t12) t2 = Node t11 a (ext t12 t2)

-- Incorrect definiton of ordering 
--ord Leaf = True
--ord (Node t1 a t2) = allT a t1 && allge a t2

-- Correct defintion of ordering                    
ordered and Leaf = True
ordered and (Node t1 a t2) =  and (allT and (<= a) t1)
                             (and (allT and (>= a) t2)
                             (and (ordered and t1)
                                  (ordered and t2)))

--orderedTrad Leaf = True
--orderedTrad (Node t1 a t2) = andTrad (allle a t1)
--                           ( andTrad (allge a t2)
--                           ( andTrad (ordered t1)
--                           ( ordered t2)))

depth Leaf = Z 
depth (Node t1 x t2) = S (max (depth t1) (depth t2))

depthBad Leaf = Z                       
depthBad (Node t1 x t2) = max x (S (max (depth t1) (depth t2)))

--prop_ordDel n t = ord t ==> ord (del n t)


