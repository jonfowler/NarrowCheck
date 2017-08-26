module Union where

import Prelude ()
import OverlapPrelude

data List = E | C Nat List

check :: List -> List -> Result
check l l' = set l && set l' ==> set (union l l')

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
