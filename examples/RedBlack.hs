module RedBlack where

import Prelude ()
import OverlapPrelude

-- Red-Black trees in a functional setting, by Okasaki.
-- (With invariants coded, and a fault injected.)


check :: Nat -> Nat -> Tree -> Result
check k a t = redBlackN k t ==> redBlack (insert a t)

checkn :: Nat -> Nat -> Nat -> Tree -> Result
checkn n k a t = sized (redBlackN k t ==> redBlack (insert a t))
                       (maxDepth t <= n)

enumcheckn :: Nat -> Nat -> Tree -> Nat -> Result
enumcheckn n k t a
  = sized (redBlackN k t  ==> redBlack (insert a t))
          ((countReds t + (s2 ^ k)) <= n && allLE n t && (a <= n))

benchmarkn :: Nat -> Tree -> Result
benchmarkn n t = redBlackN n t && (depthNat t == Z) ==> True

redBlackn :: Nat -> Tree -> Result
redBlackn k t = blackRoot t && blackN t k && red t ==> True

countReds :: Tree -> Nat
countReds (N R t1 a t2) = s1 + countReds t1 + countReds t2
countReds (N B t1 a t2) = countReds t1 + countReds t2
countReds L = Z

data Colour = R | B

data Tree = L | N Colour Tree Nat Tree

{-# DIST L 1 #-}
{-# DIST N 2 #-}
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

blackN :: Tree -> Nat -> Bool
blackN L Z = True
blackN (N R t1 x t2) n = blackN t1 n && blackN t2 n
blackN (N B t1 x t2) (S n) = blackN t1 n && blackN t2 n
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

redBlackN k t =  blackRoot t && blackN t k && red t && ord t

redBlack t =  red t && black t && ord t

--sizeBlack t n = countBlack t == n

prop_insertRB x t = redBlack t -- ==> redBlack (insert x t)

ex1 :: Tree
ex1 = N B (N B L Z L) Z (N B L Z L)

maxDepth :: Tree -> Nat
maxDepth L = Z
maxDepth (N c t1 x t2) = S (max (maxDepth t1)  (maxDepth t2))

