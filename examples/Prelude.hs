module Prelude where

data Bool = True | False

data Nat = Z | S Nat

not x = case x of
  True -> False
  False -> True

x <= y = case x of
  Z -> True
  S x2 -> case y of
    Z -> False
    S y2 -> x2 <= y2

x < y = not (y >= x)  

x >= y = case y of
  Z -> True
  S y2 -> case x of
    Z -> False
    S x2 -> x2 >= y2

x == y = case x of
  Z -> case y of
    Z -> True
    S y2 -> False
  S x2 -> case y of
    Z -> False
    S y2 -> x2 == y2

x > y = not (x <= y) 

x + y = case x of
  Z -> y
  S x2 -> S (x2 + y)

x && y = case x of
  False -> False
  True -> y

x || y = case x of
  False -> y 
  True -> True 

x ==> y = case x of -- further test
  True -> y 
  False -> True

