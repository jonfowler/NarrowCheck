
data Tree = Leaf | Node Tree Nat Tree

reach x = (s4 <= x) || (s1 <= x)

-- testing comments
          
all t p = case t of
  Leaf -> True -- this is a test
  Node t1 x t2 -> p x && all t1 p && all t2 p

s1 = S Z

s2 = S s1 

s3 = S s2

s4 = S s3

del n t = case t of
  Leaf -> Leaf
  Node t1 a t2 -> case a < n of
    True -> Node t1 a (del n t2)
    False -> case n > a of
      True -> Node (del n t1) a t2
      False -> ext t1 t2



ext t1 t2 = case t1 of
  Leaf -> t2
  Node t11 a t12 -> Node t11 a (ext t12 t2)
