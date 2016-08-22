module Tree2 where

data Tree = Leaf | Node Tree Nat Tree

reach :: Nat -> Tree -> Bool
reach = prop_ordDel 

--reach x = (s4 <= x) || (s1 <= x)

--all :: (Nat -> Bool) -> Tree ->  Bool
allle i Leaf = True 
allle i (Node t1 x t2) = x <= i && allle i t1 && allle i t2

allge i Leaf = True
allge i (Node t1 x t2) = x >= i && allge i t1 && allge i t2

del n Leaf = Leaf
del n (Node t1 a t2) = case a < n of
    True -> Node t1 a (del n t2)
    False -> case n > a of
      True -> Node (del n t1) a t2
      False -> ext t1 t2

ext Leaf t2 = t2
ext (Node t11 a t12) t2 = Node t11 a (ext t12 t2)

ord Leaf = True
ord (Node t1 a t2) = allge a t1 && allle a t2

prop_ordDel n t = ord t ==> ord (del n t)
