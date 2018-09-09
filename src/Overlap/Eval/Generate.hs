
module Overlap.Eval.Generate where

import Overlap.Eval.Monad
import System.Random hiding (newStdGen)
import Overlap.Lens
import Data.Maybe
import Control.DeepSeq
import Control.Applicative

randomSearch :: Int -> (e -> Tree (Maybe b)) -> e -> State StdGen [Maybe b]
randomSearch bt f e = liftA2 (:) (randomS bt (f e)) (randomSearch bt f e)

takeIt :: Int -> [(Int, a)] -> (a,[(Int,a)])
takeIt i ((n, a) : l) | n > i = (a, l)
                      | otherwise = ((n,a) :) <$> takeIt (i - n) l

takeRand :: [(Int, a)] -> State StdGen (a, [(Int, a)])
takeRand l = do
  let x = sum . fmap fst $ l
  i <- state $ randomR (0,x - 1)
  return (takeIt i l)

randomS :: Int -> Tree (Maybe a) -> State StdGen (Maybe a)
randomS backtrack t = go t [] where
  go (Leaf (Just a)) _ = return (Just a)
  go (Branch bs) backups | not (null bs) = do
    (t',bs') <- takeRand bs
    go t' (take backtrack (bs' : backups))
  go _ (bs : backups) = go (Branch bs) backups
  go _ _ = return Nothing
