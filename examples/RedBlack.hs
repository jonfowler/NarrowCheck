module RedBlack where

import Prelude ()
import OverlapPrelude

-- Red-Black trees in a functional setting, by Okasaki.
-- (With invariants coded, and a fault injected.)


check :: Nat -> Nat -> Tree -> Result 
check k a t = redBlackN k t && (k <= s3) ==> True -- redBlack (insert a t)

checkn :: Nat -> Nat -> Nat -> Tree -> Result 
checkn n k a t = redBlackN k t && (k <= n) ==> True -- redBlack (insert a t)

benchmarkn :: Nat -> Tree -> Result
benchmarkn n t = redBlackN n t && (depthNat t == Z) ==> True

redBlackn :: Nat -> Tree -> Result
redBlackn k t = blackRoot t && blackN k t && red t ==> True


data Colour = R | B

data Tree = L | N Colour Tree Nat Tree

{-# DIST L 1 #-}
{-# DIST N 3 #-}
-- Methods

member x L = False
member x (N col a y b) = if' (x < y) 
  (member x a)
  (if' (x > y) 
    (member x b)
    True)

makeBlack (N col a y b) = N B a y b

insert x s = makeBlack (ins x s)

ins x L = N R L x L
ins x (N col a y b) = if' (x < y) 
  (balance col (ins x a) y b)
  (if' (x > y) 
    (balance col a y (ins x b))
    (N col a y b))

-- Mistake on 4th line, 3rd line is correct
balance B (N R (N R a x b) y c) z d = N R (N B a x b) y (N B c z d)
balance B (N R a x (N R b y c)) z d = N R (N B a x b) y (N B c z d)
--balance B a x (N R (N R b y c) z d) = N R (N B a x b) y (N B c z d)
balance B a x (N R (N R c y b) z d) = N R (N B a x b) y (N B c z d)
balance B a x (N R b y (N R c z d)) = N R (N B a x b) y (N B c z d)
balance col a x b = N col a x b

-- Helpers

isRed R = True
isRed B = False

blackRoot L = True
blackRoot (N B a x b) = True
blackRoot x = False

-- INVARIANT 1. No red node has a red parent.

red L = True
red (N col a x b) = ((isRed col) ===> (blackRoot a && blackRoot b)) 
    && red a && red b

-- INVARIANT 2. Every path from the root to an empty node contains the
-- same number of black nodes.

maxBlack L = Z
maxBlack (N R t1 x t2) = max (maxBlack t1) (maxBlack t2)
maxBlack (N B t1 x t2) = S (max (maxBlack t1) (maxBlack t2))

black t = fst (black' t)

black' L = T True Z
black' (N c t1 x t2) = black'' c (black' t1) (black' t2) 

black'' R (T b1 m) (T b2 n) = T (b1 && b2 && (m == n)) (max m n)
black'' B (T b1 m) (T b2 n) = T (b1 && b2 && (m == n)) (S (max m n))

ifB R m n = n 
ifB B m n = m 

blackN :: Nat -> Tree -> Bool
blackN Z L = True
blackN n (N R t1 x t2) = blackN n t1 && blackN n t2
blackN (S n) (N B t1 x t2) = blackN n t1 && blackN n t2
blackN n t = False


div2 Z = Z
div2 (S Z) = S Z
div2 (S (S x)) = S (div2 x)

-- INVARIANT 3. Trees are ordered.

allLE x L = True
allLE x (N col t0 a t1) = (a <= x) && allLE x t0 && allLE x t1

allGE x L = True
allGE x (N col t0 a t1) = (a >= x) && allGE x t0 && allGE x t1

ord L = True
ord (N col t0 a t1) =  allLE a t0 && allGE a t1 && ord t0 && ord t1

-- Properties

smax L = True
smax (N c t0 a t1) = a <= s4 && smax t0 && smax t1

depthNat :: Tree -> Nat
depthNat L = Z
depthNat (N c t0 a t1) = max a (max (depthNat t0) (depthNat t1))

redBlackN k t =  blackRoot t && blackN k t && red t && ord t

redBlack t =  red t && black t && ord t

--sizeBlack t n = countBlack t == n

prop_insertRB x t = redBlack t -- ==> redBlack (insert x t)

ex1 :: Tree
ex1 = N B (N B L Z L) Z (N B L Z L)

nodeDepth :: Tree -> Nat -> Bool
nodeDepth L x = True
nodeDepth (N c t1 x t2) (S n) = nodeDepth t1 n  && nodeDepth t2 n
nodeDepth x a = False

