module Overlap.Eval.Enumerate where

import Overlap.Eval.Monad
import Data.Maybe

enumerate :: (a -> Maybe b) -> Tree a -> [b]
enumerate f (Leaf a) = maybeToList (f a)
enumerate f (Branch ts) = concatMap (enumerate f . snd) ts
