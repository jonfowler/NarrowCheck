module PermTrad where

import Prelude ()
import OverlapPrelude

length E = Z
length (C a l) = S (length l)

perm n l = (n == length l) *&&* allLT n l *&&* allDiff l

allLT n E = True
allLT n (C n' l) = (n' < n) *&&* allLT n l

allDiff E = True
allDiff (C n l) = notIn n l *&&* allDiff l

notIn n E = True
notIn n (C n' l) = (n /= n') *&&* notIn n l

sort E = E
sort (C a l) = sortRec a (splitOn a l E E)

sortRec :: Nat -> Tuple (List Nat) (List Nat) -> List Nat
sortRec a (T l1 l2) = sort l1 ++ (C a (sort l2))

splitOn a E l1 l2 = T l1 l2
splitOn a (C b l) l1 l2 = if' (a < b) (splitOn a l (C a l1) l2) (splitOn a l l1 (C a l2))

checkn :: Nat -> List Nat -> Result
checkn n l = perm n l ==> eqListTrad (==) l (sort l)

check :: List Nat -> Result
check l = checkn s9 l
