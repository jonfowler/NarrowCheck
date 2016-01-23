module RedBlack1 where

-- Red-Black trees in a functional setting, by Okasaki.
-- (With invariants coded, and a fault injected.)

data Colour = R | B

data Tree = E | T Colour Tree Nat Tree

-- Methods

member x E = False
member x (T col a y b) = case x < y of
  True -> member x a
  False -> case x > y of
    True -> member x b
    False -> True

makeBlack (T col a y b) = T B a y b

insert x s = makeBlack (ins x s)

ins x E = T R E x E
ins x (T col a y b) = case x < y of
  True -> balance col (ins x a) y b
  False -> case x > y of
    True -> balance col a y (ins x b)
    False -> T col a y b

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
red (T col a x b) = (case isRed col of
  True -> blackRoot a && blackRoot b
  False -> True) && red a && red b

-- INVARIANT 2. Every path from the root to an empty node contains the
-- same number of black nodes.

data Pair = Pair Bool Nat

black t = case black' t of
  Pair b n -> b

black' E = Pair True (S Z)
black' (T col a x b) = case black' a of
  Pair b0 n -> case black' b of
    Pair b1 m -> Pair (b0 && b1 && (n == m)) (n + case isRed col of
                                                     True -> Z
                                                     False -> S Z)

-- INVARIANT 3. Trees are ordered.

allLe x E = True
allLe x (T col t0 a t1) = (a <= x) && allLe x t0 && allLe x t1

allGe x E = True
allGe x (T col t0 a t1) = (a >= x) && allGe x t0 && allGe x t1

ord E = True
ord (T col t0 a t1) = allLe a t0 && allGe a t1 && ord t0 && ord t1

-- Properties

--infixr 0 -->
--False --> _ = True
--True --> x = x

redBlack t = ord t && black t && red t

-- refute
prop_insertRB x t = (redBlack t ==> redBlack (insert x t))

reach = prop_insertRB 


