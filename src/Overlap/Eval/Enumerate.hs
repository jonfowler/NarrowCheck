module Overlap.Eval.Enumerate where

import Overlap.Eval.Monad
import Data.Maybe

enumerate :: (a -> Maybe b) -> Tree a -> ([b],Int)
enumerate f (Leaf a) = (maybeToList (f a), 0)
enumerate f (Branch ts) = if null res then (res,1 + cnt) else (res,cnt)
   where enms = map (enumerate f . snd) ts
         res = concatMap fst enms
         cnt = sum (map snd enms)
