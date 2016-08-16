module ReachPrelude where

import Prelude (Show)

data Bool = True | False deriving Show

data Nat = Z | S Nat deriving Show

not False = True
not True = False

Z <= y = True
S x <= Z = False
S x <= S y = x <= y

--x < y = case y of
--  Z -> False 
--  S y' -> case x of
--    Z -> True
--    S x' -> x' < y'

Z > y = False
S x > S y = x > y
S x > Z = True

x < y = y > x

x >= y = y <= x

Z == Z = True 
Z == S x = False
S x == Z = False
S x == S y = x == y

x /= y = not (x == y)

Z + y = y
x + Z = x
S x + y = S (x + y)
x + S y = S (x + y)


False && y = False
x && False = False
True && y = y
x && True = x

False || y = y
x || False = x
True || y = True
x || True = True

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
s9 = S s8
s10 = S s9
s11 = S s10
s12 = S s11

