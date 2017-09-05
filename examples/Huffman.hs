module Huffman where

import Prelude(Show)
import OverlapPrelude

data Tree = Leaf Char | Fork Tree Tree deriving Show
data Char = U | V | W | X | Y deriving Show

checkBasic :: List Char -> Result
checkBasic l = not (null l) ==> eqListTrad (=|=) l (encdec l (hufftree l)) 

encdec :: List Char -> Tree -> List Char
encdec l t = decode t (encode t l)

nothing :: List Char -> Bool
nothing a = True

U =|= U = True
V =|= V = True
W =|= W = True
X =|= X = True
Y =|= Y = True
x =|= y = False

decode :: Tree -> List Bool -> List Char
decode t E =  E
decode t bs = dec t t bs

dec :: Tree -> Tree -> List Bool -> List Char
dec (Leaf x) t bs = C x (decode t bs)
dec (Fork xt yt) t (C False bs) = dec xt t bs
dec (Fork xt yt) t (C True bs) = dec yt t bs

encode :: Tree -> List Char -> List Bool
encode t cs = enc (codetable t) cs

enc :: List (Tuple Char (List Bool)) -> List Char -> List Bool
enc table E = E
enc table (C c cs) = (charLookup c table) ++ enc table cs

charLookup :: Char -> (List (Tuple Char b)) -> b
charLookup n (C (T n' b) l) = if' (n =|= n') b (charLookup n l)

collate :: List Char -> List (Tuple Nat Tree)
collate E = E
collate (C c cs) = collate' c (count c cs)

collate' c nl = insert (T (S (fst nl)) (Leaf c)) (collate (snd nl))

--  where (n, ds) = count c cs
--
count :: Char -> List Char -> Tuple Nat (List Char)
count x E = T Z E
count x (C y ys) = count' x y (count x ys)


count' :: Char -> Char -> Tuple Nat (List Char) -> Tuple Nat (List Char)
count' x y nzs = if' (x =|= y) (T (S (fst nzs)) (snd nzs)) (T (fst nzs) (C y (snd nzs)))

insert :: Tuple Nat a -> List (Tuple Nat a) -> List (Tuple Nat a)
insert t E = singleton t
insert (T n c) (C (T n' c') l) = if' (n <= n') (T n c +: (T n' c' +: l)) (T n' c' +: insert (T n c) l)

hufftree cs = mkHuff (collate cs)
--
mkHuff (C (T n t) E) = Fork t (Leaf Y)
mkHuff l = mkHuff' l

mkHuff' (C (T n t) E) = t
mkHuff' (C (T n0 t0) (C (T n1 t1) wts)) =
  mkHuff' (insert (T (n0 + n1) (Fork t0 t1)) wts)

--((x, bs) : xbs) ! y = if x *=* y then bs else xbs ! y
--
codetable :: Tree -> List (Tuple Char (List Bool))
codetable t = tab E t

tab :: List Bool -> Tree -> List (Tuple Char (List Bool))
tab p (Leaf x) = singleton (T x p)
tab p (Fork xt yt) = tab (p++ singleton False) xt ++ tab (p ++ singleton True) yt
--
--collate [] = []

