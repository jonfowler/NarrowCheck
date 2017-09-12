module OverlapPrelude where

import Prelude (Show)

data Bool = True | False deriving Show


{-# DIST Z 1 #-}
{-# DIST S 2 #-}

not False = True
not True = False

if' True x y = x
if' False x y = y

-- Traditional and
False *&&* y = False
True *&&* y = y

{-# OVERLAP (&&) #-}
False && y = False
x && False = False
True && y = y
x && True = x

{-# OVERLAP (||) #-}
False || y = y
x || False = x
True || y = True
x || True = True

{-# OVERLAP (===>) #-}
True ===> y = y
x ===> False = not x
False ===> y = True
x ===> True = True

data Result = NoTest | Fail | Success

False ==> x = NoTest
True ==> False = Fail
True ==> True = Success

False ==*> x = NoTest 
True ==*> x = x

post False = Fail
post True = Success

data Nat = Z | S Nat deriving Show

normaliseNat :: Nat -> Bool
normaliseNat Z = True
normaliseNat (S n) = normaliseNat n

Z <= y = True
S x <= Z = False
S x <= S y = x <= y

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

{-# OVERLAP (+) #-}
Z + y = y
x + Z = x
S x + y = S (x + y)
x + S y = S (x + y)

pred Z = Z
pred (S x) = x

maxTrad Z y = y
maxTrad (S x) y = S (maxTrad x (pred y))

{-# OVERLAP max #-}
max Z y = y
max x Z = x
max (S x) y = S (max x (pred y))
max x (S y) = S (max (pred x) y)

andTrad False x = False
andTrad True x = x

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

s20 = s10 + s10
s30 = s20 + s10
s40 = s30 + s10

flip :: (a -> b -> c) -> b -> a -> c
flip f b a = f a b

{-
LIST Functions
-}

data Tuple a b = T a b deriving Show

fst :: Tuple a b -> a
fst (T a b) = a

snd :: Tuple a b -> b
snd (T a b) = b

data List a = E | C a (List a) deriving Show

data Maybe a = Nothing | Just a deriving Show

a +: l = C a l

length :: List a -> Nat
length E = Z
length (C a l) = S (length l)

null :: List a -> Bool
null E = True
null l = False

foldr :: (a -> b -> b) -> b -> List a -> b
foldr f b E = b
foldr f b (C a l) = f a (foldr f b l)

all :: (a -> Bool) -> List a -> Bool
all p E = True
all p (C a l) = p a && all p l

allTrad :: (a -> Bool) -> List a -> Bool
allTrad p E = True
allTrad p (C a l) = p a *&&* allTrad p l

lookup :: Nat -> (List (Tuple Nat b)) -> Maybe b
lookup n E = Nothing
lookup n (C (T n' b) l) = if' (n == n') (Just b) (lookup n l)

singleton :: a -> List a
singleton a = C a E

eqList :: (a -> a -> Bool) -> List a -> List a -> Bool
eqList p E E = True
eqList p (C a l) (C a' l') = p a a' && eqList p l l'
eqList p a b = False

eqListTrad :: (a -> a -> Bool) -> List a -> List a -> Bool
eqListTrad p E E = True
eqListTrad p (C a l) (C a' l') = p a a' *&&* eqListTrad p l l'
eqListTrad p a b = False


E ++ l = l
(C a l) ++ l' = C a (l ++ l')
