module Perm where

import Prelude ()
import OverlapPrelude

checkn :: Nat -> List Nat -> Result
checkn n l = perm n l ==> eqList (==) (ascend n) (sort l)

ascend Z = E
ascend (S n) = C Z (map S (ascend n))

sort E = E
sort (C a l) = sortRec a (splitOn a l E E)

sortRec :: Nat -> Tuple (List Nat) (List Nat) -> List Nat
sortRec a (T l1 l2) = sort l1 ++ (C a (sort l2))

splitOn a E l1 l2 = T l1 l2
splitOn a (C b l) l1 l2 = if' (a < b) (splitOn a l l1 (C b l2)) (splitOn a l (C b l1) l2)

check :: List Nat -> Result
check l = checkn s3 l

genn :: Nat -> List Nat -> Bool
genn n l = perm n l

perm :: Nat -> List Nat -> Bool
perm n l = (n == length l) && all (< n) l && allDiff l

allDiff E = True
allDiff (C n l) = all (n /=) l && allDiff l

