module Union where

import Prelude ()
import OverlapPrelude

data List = E | C Nat List

{-# Dist E 1 #-}
{-# Dist C 6 #-}

check :: List -> List -> Result
check l l' = gen l l'  ==> set (union l l')

basic :: List -> List -> Result
basic l l' = normaliseList l *&&* normaliseList l' ==*> check l l'

gen :: List -> List -> Bool
gen l l' = set l *&&* set l'

genBasic :: List -> List -> Bool
genBasic l l' = normaliseList l *&&* normaliseList l' *&&* gen l l'

set :: List -> Bool
set E = True
set (C a l) = set' a l

set' :: Nat -> List -> Bool
set' a E = True
set' a (C a' l) = (a < a') && set' a' l

union :: List -> List -> List
union E l = l
union l E = l
union (C a l) (C a' l') = if' (a < a') (C a  (union l (C a' l')))
                                       (C a' (union (C a l) l'))
normaliseList :: List -> Bool
normaliseList E = True
normaliseList (C a l) = normaliseNat a *&&* normaliseList l

