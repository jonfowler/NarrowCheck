module Perm where


data List = C Nat List
          | E

length E = Z
length (C a l) = S (length l)

perm n l = (n == length l) && allLT n l && allDiff l

allLT n E = True
allLT n (C n' l) = (n' < n) && allLT n l

allDiff E = True
allDiff (C n l) = notIn n l && allDiff l

notIn n E = True
notIn n (C n' l) = (n /= n') && notIn n l

reach :: List -> Bool
reach l = perm s7 l
