module Reverse where

import Prelude ()
import OverlapPrelude

{-# DIST E 1 #-}
{-# DIST C 5 #-}

checkBasic :: List Nat -> List Nat -> Result
checkBasic l l' = post (eqListTrad (==) (reverse (l ++ l')) (reverse l' ++ reverse l))

reverse :: List a -> List a
reverse l = reverse' l E

reverse' :: List a -> List a -> List a
reverse' E l = l
reverse' (C a l) l' = reverse' l (C a l')
