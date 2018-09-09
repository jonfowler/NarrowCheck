module NQueensTrad where

import Prelude ()
import OverlapPrelude

nqueens :: Nat -> List Nat -> Result
nqueens n l
   = (n == length l)
   *&&* all (< n) l
   *&&* allDiff l
   *&&* diags (map (n -) l)
   *&&* diags l ==> True

diags :: List Nat -> Bool
diags E = True
diags (C n l) = checkLowering n l && diags l

checkLowering n E = True
checkLowering Z l = True
checkLowering (S n) (C a l) = if' (n == a) False (checkLowering n l)

allDiff E = True
allDiff (C n l) = all (n /=) l && allDiff l


