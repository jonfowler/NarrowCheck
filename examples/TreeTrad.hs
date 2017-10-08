module TreeTrad where

import Prelude ()
import OverlapPrelude

data Tree = Leaf | Node Tree Nat Tree

{-# DIST Leaf 1 #-}
{-# DIST Node 1 #-}

{-
The following properties all use overlapping conjunction in order
to utilise the sizing functions.
-}
checkn :: Nat -> Nat -> Tree -> Result
checkn i n t = (ordered t && (depth t <= i) && (depthNat t <= s30))
                                      ==> ordered (del n t) 

check :: Nat -> Tree -> Result
check n t = checkn s5 n t

checkBasic :: Nat -> Tree -> Result
checkBasic n t = ordered t ==> ordered (del n t)

altCheckBasic :: Nat -> Tree -> Result
altCheckBasic n t = altOrdered t ==> altOrdered (del n t)

genn :: Nat -> Tree -> Bool
genn i t = ordered t && (depth t <= i) && (depthNat t <= s30)

enumCheckn :: Nat -> Tree -> Result
enumCheckn i t = (ordered t && (depth t <= i) && (depthNat t <= s3))
                                      ==> True

enumBalancedn :: Nat -> Tree -> Result
enumBalancedn i t = (ordered t && balancedTree i t && (depthNat t < s6))
                                      ==> True

 {-
The following functions have been converted to use traditional conjunction 
-}

depthNat :: Tree -> Nat
depthNat Leaf = Z
depthNat (Node t1 a t2) = max a (max (depthNat t1) (depthNat t2))

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
                          ordered t1 *&&*
                          allge a t2 *&&*
                          ordered t2

altOrdered Leaf = True
altOrdered (Node t1 a t2) = altOrdered t1  *&&*
                            altAll (<= a) t1 *&&*
                            altAll (>= a) t2 *&&*
                            altOrdered t2

altAll :: (Nat -> Bool) -> Tree -> Bool
altAll p (Node t1 a t2) = altAll p t1 *&&* p a *&&* altAll p t2
altAll p Leaf = True



depth Leaf = Z 
depth (Node t1 x t2) = S (maxTrad (depth t1) (depth t2))

--prop_ordDel n t = ord t ==> ord (del n t)

balancedTree :: Nat -> Tree -> Bool
balancedTree Z Leaf = True
balancedTree (S n) (Node t1 a t2) = balancedTree (halfup n) t1 && balancedTree (halfdown n) t2
balancedTree n t = False

halfup :: Nat -> Nat
halfup Z = Z
halfup (S Z) = S Z
halfup (S (S x)) = S (halfup x)

halfdown :: Nat -> Nat
halfdown Z = Z
halfdown (S Z) = Z 
halfdown (S (S x)) = S (halfdown x)
