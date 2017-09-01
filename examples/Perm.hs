module Perm where

import Prelude ()
import OverlapPrelude

checkn :: Nat -> List Nat -> Result
checkn n l = perm n l ==> True

check :: List Nat -> Result
check l = checkn s9 l

genn :: Nat -> List Nat -> Bool
genn n l = perm n l

perm n l = (n == length l) && all (< n) l && allDiff l

allDiff E = True
allDiff (C n l) = all (n /=) l && allDiff l

