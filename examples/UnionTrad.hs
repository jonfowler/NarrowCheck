module UnionTrad where

import Prelude ()
import OverlapPrelude

{-# Dist E 1 #-}
{-# Dist C 5 #-}

checkBasic :: List Nat -> List Nat -> Result 
checkBasic l l' = gen l l'  ==> set (union l l')

checkn :: Nat -> List Nat -> List Nat -> Result
checkn n l l' = sized
  (set l && set l' ==> set (union l l'))
  ((length l <= n) && (length l' <= n) && all (<= n) l && all (<= n) l')

gen :: List Nat -> List Nat -> Bool
gen l l' = set l *&&* set l'

set :: List Nat -> Bool
set E = True
set (C a l) = set' a l

set' :: Nat -> List Nat -> Bool
set' a E = True
set' a (C a' l) = (a < a') *&&* set' a' l

union :: List Nat -> List Nat -> List Nat
union E l = l
union l E = l
union (C a l) (C a' l') = if' (a < a') (C a  (union l (C a' l')))
                                       (C a' (union (C a l) l'))
