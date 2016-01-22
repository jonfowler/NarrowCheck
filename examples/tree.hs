module Tree where

data Tree = Leaf | Node Tree Nat Tree

reach = prop_ordDel 

--reach x = (s4 <= x) || (s1 <= x)

s1 = S Z

s2 = S s1 

s3 = S s2

s4 = S s3

--all :: (Nat -> Bool) -> Tree ->  Bool
all p Leaf = True 
all p (Node t1 x t2) = p x && all p t1 && all p t2

del n Leaf = Leaf
del n (Node t1 a t2) = case a < n of
    True -> Node t1 a (del n t2)
    False -> case n > a of
      True -> Node (del n t1) a t2
      False -> ext t1 t2

ext Leaf t2 = t2
ext (Node t11 a t12) t2 = Node t11 a (ext t12 t2)

ord Leaf = True
ord (Node t1 a t2) = all (a >=) t1 && all (a <=) t2

prop_ordDel n t = ord t ==> ord (del n t)
