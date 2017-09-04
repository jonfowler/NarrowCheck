module Union where

import Prelude ()
import OverlapPrelude

{-# Dist E 1 #-}
{-# Dist C 6 #-}

check :: List Nat -> List Nat -> Result
check l l' = gen l l'  ==> set (union l l')

basic :: List Nat -> List Nat -> Result
basic l l' = normaliseList l *&&* normaliseList l' ==*> check l l'

gen :: List Nat -> List Nat -> Bool
gen l l' = set l *&&* set l'

genBasic :: List Nat -> List Nat -> Bool
genBasic l l' = normaliseList l *&&* normaliseList l' *&&* gen l l'

set :: List Nat -> Bool
set E = True
set (C a l) = set' a l

set' :: Nat -> List Nat -> Bool
set' a E = True
set' a (C a' l) = (a < a') && set' a' l

union :: List Nat -> List Nat -> List Nat
union E l = l
union l E = l
union (C a l) (C a' l') = if' (a < a') (C a  (union l (C a' l')))
                                       (C a' (union (C a l) l'))
normaliseList :: List Nat -> Bool
normaliseList E = True
normaliseList (C a l) = normaliseNat a *&&* normaliseList l

