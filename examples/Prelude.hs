module Prelude where

--import GHC.Show

data Bool = True | False 

data Nat = Z | S Nat

not x = case x of
  True -> False
  False -> True

Z <= y = True
S x <= Z = False
S x <= S y = x <= y

x < y = case y of
  Z -> False 
  S y' -> case x of
    Z -> True
    S x' -> x' < y'
--Z < Z = False
--Z < S x = True
--S x < Z = False
--S x < S y = x < y

x >= y = y <= x

x == y = case x of
  Z -> case y of
    Z -> True
    S y2 -> False
  S x2 -> case y of
    Z -> False
    S y2 -> x2 == y2

x /= y = not (x == y)

x > y = y < x 

x + y = case x of
  Z -> y
  S x2 -> S (x2 + y)

False && y = False
True && False = False
True && True = True 

x || y = case x of
  False -> y 
  True -> True 

False ==> x = True -- further test
True ==> x = x 

s1 = S Z
s2 = S s1 
s3 = S s2
s4 = S s3
s5 = S s4
s6 = S s5
s7 = S s6
s8 = S s7

