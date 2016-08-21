module RedBlack where

import Prelude ()
import ReachPrelude

-- Red-Black trees in a functional setting, by Okasaki.
-- (With invariants coded, and a fault injected.)

data Colour = R | B

data Tree = E | T Colour Tree Nat Tree

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
blackRoot (T col a x b) = not (isRed col)

-- INVARIANT 1. No red node has a red parent.

red E = True
red (T col a x b) = if' (isRed col) (blackRoot a && blackRoot b) True
                  && red a
                  && red b

-- INVARIANT 2. Every path from the root to an empty node contains the
-- same number of black nodes.

data Pair = Pair Bool Nat

pair (Pair x y) f = f x y

countBlack E = Z
countBlack (T R t1 x t2) = max (countBlack t1) (countBlack t2)
countBlack (T B t1 x t2) = S (max (countBlack t1) (countBlack t2))

black E = True
black (T c t1 x t2) = (countBlack t1 == countBlack t2) && black t1 && black t2 

blackN E Z = True
blackN E (S n) = False
blackN (T R t1 x t2) n = blackN t1 n && blackN t2 n
blackN (T B t1 x t2) (S n) = blackN t1 n && blackN t2 n 
blackN (T B t1 x t2) Z = False
                                 
maxLength E = Z
maxLength (T c t1 x t2) = S (max (maxLength t1) (maxLength t2))

balanced E = True
balanced (T c t1 x t2) = balanced t1 && balanced t2 &&
        ((div2 (maxLength t1) <= maxLength t2)
         || (div2 (maxLength t2) <= (maxLength t1)))

div2 Z = Z
div2 (S Z) = S Z
div2 (S (S x)) = S (div2 x)

-- INVARIANT 3. Trees are ordered.

allLe x E = True
allLe x (T col t0 a t1) = (a <= x) && allLe x t0 && allLe x t1

allGe x E = True
allGe x (T col t0 a t1) = (a >= x) && allGe x t0 && allGe x t1

ord E = True
ord (T col t0 a t1) = allLe a t0 && allGe a t1 && ord t0 && ord t1

-- Properties

smax E = True
smax (T c t0 a t1) = a < s4 && smax t0 && smax t1

redBlack t =  black t && red t && ord t

sizeBlack t n = countBlack t == n
--              && balanced t
-- ord t &&

-- refute
prop_insertRB x t = redBlack t -- ==> redBlack (insert x t)

ex1 :: Tree
ex1 = T B (T B E Z E) Z (T B E Z E)

reach :: Nat -> Tree -> Bool
reach n t = (blackN t n) && redBlack t && smax t

  --prop_insertRB

  --black t ==> balanced
--T (R)
--  (T (B) (E) (_) (E))
--  (_)
--  (T (B)
--      (T (R) (E) (_) (E))
--      (_)
--      (T (R)
--         (E)
--         (_)
--         (T (_) (_) (_) (_))))
