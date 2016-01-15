module RedBlack where

import Prelude

data Colour = R | B

data Tree = E | T Colour Tree Nat Tree

-- Methods

member x t = case t of
  E -> False
  T _ a y b -> case x < y of
    True -> member x a
    False -> case y < x of
      True -> member x b
      False -> True

makeBlack t = case t of
  T _ a y b -> T B a y b

insert x s = makeBlack (ins x s)

ins x t = case t of
  E -> T R E x E
  T col a y b -> case x < y of
    True -> balance col (ins x a) y b
    False -> case y < x of
      True -> balance col a y (ins x b)
      False -> T col a y b

-- Mistake on 4th line, 3rd line is correct
--balance col t1 z t2 = case col of
--  B -> case t1 of
--    T col1 t3 x t4 = case col1 of
--      R -> case 
--      B -> case t2 of
--        T col2 t5 x' t6 ->
--
--        E -> T col t1 z t2
--    E -> T col t1 z t2  
--  R -> T col t1 z t2


balance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
--balance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balance B a x (T R (T R c y b) z d) = T R (T B a x b) y (T B c z d)
balance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balance col a x b = T col a x b

-- Helpers

isRed col = case col of
  R -> True
  B -> False

--blackRoot E = True
--blackRoot (T col a x b) = not (isRed col)
--
---- INVARIANT 1. No red node has a red parent.
--
--red E = True
--red (T col a x b) =
--  (if isRed col then blackRoot a && blackRoot b else True) && red a && red b
--
---- INVARIANT 2. Every path from the root to an empty node contains the
---- same number of black nodes.
--
--
--black t = fst (black' t)
--
--black' E = (True, S Z)
--black' (T col a x b) = (b0 && b1 && n *=* m, n *+* if isRed col then Z else S Z)
--  where (b0, n) = black' a
--        (b1, m) = black' b
--
---- INVARIANT 3. Trees are ordered.
--
--allLe x E = True
--allLe x (T _ t0 a t1) = a *<=* x && allLe x t0 && allLe x t1
--
--allGe x E = True
--allGe x (T _ t0 a t1) = a *>=* x && allGe x t0 && allGe x t1
--
--ord E = True
--ord (T _ t0 a t1) = allLe a t0 && allGe a t1 && ord t0 && ord t1
--
---- Properties
--
--infixr 0 -->
--False --> _ = True
--True --> x = x
--
--redBlack t = ord t && black t && red t
--
--prop_insertRB (x, t) = refute (redBlack t --> redBlack (insert x t))
--
--main = prop_insertRB
