module Overlap.Eval.Enumerate where

import Overlap.Eval.Monad
import Data.Maybe

enumerate :: (a -> Maybe b) -> Tree a -> ([b],Int,Int)
enumerate f (Leaf a) = let p = f a in (maybeToList p, 0, if isNothing p then 1 else 0)
enumerate f (Branch ts) = (res, if null res then 1 + cnt else cnt, sum . map trd3 $ enms)
   where enms = map (enumerate f . snd) ts
         res = concatMap fst3 enms
         cnt = sum (map snd3 enms)

fst3 (x,_,_) = x
snd3 (_,y,_) = y
trd3 (_,_,z) = z
