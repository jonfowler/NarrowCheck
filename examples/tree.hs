

data Bool = True | False
data Nat = Z | S Nat
data Tree = Leaf | Node Tree Nat Tree

reach x = all x (le s1) 

all t p = case t of
  Leaf -> True
  Node t1 x t2 -> and (p x) (and (all t1 p) (all t2 p))

and x y = case x of
  True -> y
  False -> False

imp x y = case x of
  True -> y
  False -> True

not x = case x of
  True -> False
  False -> True

le x y = case x of
  Z -> True
  S x2 -> case y of
    Z -> False
    S y2 -> le x2 y2

add x y = case x of
  Z -> y 
  S x2 -> S (add x2 y) 

s1 = S Z

s2 = S s1 

s3 = S s2

