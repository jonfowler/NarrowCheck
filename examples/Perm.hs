module Perm where

import Prelude ()
import OverlapPrelude

data List a = E | C a (List a)

checkn :: Nat -> List Nat -> Result
checkn n l = perm n l ==> True

check :: List Nat -> Result
check l = checkn s9 l

genn :: Nat -> List Nat -> Bool
genn n l = perm n l

length :: List a -> Nat
length E = Z
length (C a l) = S (length l)

perm n l = (n == length l) && allLT n l && allDiff l

allLT n E = True
allLT n (C n' l) = (n' < n) && allLT n l

allDiff E = True
allDiff (C n l) = notIn n l && allDiff l

notIn n E = True
notIn n (C n' l) = (n /= n') && notIn n l

