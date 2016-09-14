module Perm where

import OverlapPrelude
import Prelude ()

data List = E | C Nat List 

length E = Z
length (C a l) = S (length l)

perm n l = (n == length l) && allLT n l && allDiff l

permTrad n l = andTrad (n == length l) (andTrad (allLT n l) (allDiff l))

--sort :: List -> List           
--sort E = E
--sort (C a l) = filterLT a   &&   filterGT a

allLT n E = True
allLT n (C n' l) = (n' < n) && allLT n l

allDiff E = True
allDiff (C n l) = notIn n l && allDiff l

notIn n E = True
notIn n (C n' l) = (n /= n') && notIn n l

reach :: List -> Result
--reach l = permTrad s9 l ==> True
reach l = perm s9 l ==> True
