module RedBlack where

import Prelude ()
import OverlapPrelude

-- Red-Black trees in a functional setting, by Okasaki.
-- (With invariants coded, and a fault injected.)

data Colour = R | B

data Tree = E | T Colour Tree Nat Tree

{-# DIST E 1 #-}
{-# DIST T 3 #-}
-- Methods

member x E = False
member x (T col a y b) = if' (x < y) 
  (member x a)
  (if' (x > y) 
    (member x b)
    True)

makeBlack (T col a y b) = T B a y b

insert x s = makeBlack (ins x s)

ins x E = T R E x E
ins x (T col a y b) = if' (x < y) 
  (balance col (ins x a) y b)
  (if' (x > y) 
    (balance col a y (ins x b))
    (T col a y b))

-- Mistake on 4th line, 3rd line is correct
balance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
--balance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balance B a x (T R (T R c y b) z d) = T R (T B a x b) y (T B c z d)
balance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balance col a x b = T col a x b

-- Helpers

isRed R = True
isRed B = False

blackRoot E = True
blackRoot (T B a x b) = True
blackRoot x = False

-- INVARIANT 1. No red node has a red parent.

red and E = True
red and (T col a x b) = and
    (if' (isRed col) (and (blackRoot a) (blackRoot b)) True)
    (and (red and a) (red and b))

-- INVARIANT 2. Every path from the root to an empty node contains the
-- same number of black nodes.

data Pair = Pair Bool Nat

pair (Pair x y) f = f x y
fst (Pair x y) = x

maxBlack E = Z
maxBlack (T R t1 x t2) = max (maxBlack t1) (maxBlack t2)
maxBlack (T B t1 x t2) = S (max (maxBlack t1) (maxBlack t2))

black t = fst (black' t)

black' E = Pair True Z
black' (T c t1 x t2) = black'' c (black' t1) (black' t2) 

black'' R (Pair b1 m) (Pair b2 n) = Pair (b1 && b2 && (m == n)) (max m n)
black'' B (Pair b1 m) (Pair b2 n) = Pair (b1 && b2 && (m == n)) (S (max m n))

ifB R m n = n 
ifB B m n = m 

blackN and E Z = True
blackN and E (S n) = False
blackN and (T R t1 x t2) n = and (blackN and t1 n) (blackN and t2 n)
blackN and (T B t1 x t2) (S n) = and (blackN and t1 n) (blackN and t2 n )
blackN and (T B t1 x t2) Z = False


div2 Z = Z
div2 (S Z) = S Z
div2 (S (S x)) = S (div2 x)

-- INVARIANT 3. Trees are ordered.

allLe x E = True
allLe x (T col t0 a t1) = (a <= x) && allLe x t0 && allLe x t1

allT and p E = True
allT and p (T col t0 a t1) = and (p a) (and (allT and p t0) (allT and p t1))

allGe x E = True
allGe x (T col t0 a t1) = (a >= x) && allGe x t0 && allGe x t1

ord and E = True
ord and (T col t0 a t1) =  and (allT and (<= a) t0)
                          (and (allT and (a <=) t1)
                          (and (ord and t0)
                               (ord and t1)))

-- Properties

smax E = True
smax (T c t0 a t1) = a <= s4 && smax t0 && smax t1

redBlackN and t k =  and (blackRoot t)
                    (and (blackN and t k)
                    (and (red and t)
                         (ord and t)))

redBlack and t =  and (red and t)
                 (and (black t)
                      (ord and t))

--sizeBlack t n = countBlack t == n

prop_insertRB x t = redBlack t -- ==> redBlack (insert x t)

ex1 :: Tree
ex1 = T B (T B E Z E) Z (T B E Z E)

nodeDepth :: Tree -> Nat -> Bool
nodeDepth E x = True
nodeDepth (T c t1 x t2) (S n) = nodeDepth t1 n  && nodeDepth t2 n
nodeDepth x a = False

check :: Nat -> Nat -> Tree -> Result 
--check k a t = (redBlackN andTrad t k && (k == s3) && nodeDepth t s5) ==> True -- redBlack (insert a t)
check k a t = (redBlackN (&&) t k && (k <= s1)) ==> True -- redBlack (insert a t)

