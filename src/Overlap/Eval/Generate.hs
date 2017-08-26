
module Overlap.Eval.Generate where

import Overlap.Eval.Monad
import System.Random hiding (newStdGen)
import Overlap.Lens
import Data.Maybe

newStdGen :: State StdGen StdGen
newStdGen = do
  (n, n') <- split <$> get
  put n
  return n'

generating :: Int -> Int -> (a -> Maybe b) -> Tree a -> StateT StdGen (Writer (Sum Int)) [b]
generating n bt p = generating' n bt . Just . fmap p


generating' :: Int -> Int -> Maybe (Tree (Maybe b)) -> StateT StdGen (Writer (Sum Int)) [b]
generating' _ _ Nothing = return []
generating' n _ _ | n <= 0 = return []
generating' _ _ (Just (Leaf b)) = return (maybeToList b)
generating' n bt (Just (Branch bs)) = do
   (b, t) <- brancher bt bs bt []
   case b of
     Nothing -> generating' n bt (remake t)
     Just a -> (a:) <$> generating' (n-1) bt (remake t)

type ZipTree a = [([(Int,Tree a)], Int, [(Int, Tree a)])]

remake :: ZipTree a -> Maybe (Tree a)
remake [] = Nothing
remake (([],_,[]) : ts) = remake ts
remake ((bs,_,bs') : ts) = Just $ remake' (Branch (bs ++ bs')) ts

remake' :: Tree a -> ZipTree a -> Tree a
remake' t [] = t
remake' t ((bs,fq,bs') : ts) = remake' (Branch (bs ++ (fq,t) : bs')) ts


brancher :: Int -> [(Int,Tree (Maybe b))] -> Int ->
                          ZipTree (Maybe b) -> StateT StdGen (Writer (Sum Int)) (Maybe b, ZipTree (Maybe b))
brancher _ [] 0 ts = return (Nothing, ts)
brancher _ [] _ [] = return (Nothing, [])
brancher n [] i ((bs,_,bs') : ts) = tell (Sum 1) >> brancher n (bs ++ bs') (i -1) ts
brancher n bs i ts = do
  (xs, (fq, t) : xs') <- splitRand bs
  case t of
    Leaf (Just b) -> return (Just b, ts)
    Leaf Nothing -> brancher n (xs ++ xs') i ts
    Branch bs' -> brancher n bs' (min (i+1) n) ((xs, fq,  xs') : ts)

splitRand :: MonadState StdGen m => [(Int,a)] -> m ([(Int, a)], [(Int, a)]) 
splitRand l = do
  n <- randomS (0, tot - 1)
  return $ splitty n l
    where tot = sumOf (folded . _1) l

splitty :: Int -> [(Int,a)] -> ([(Int,a)],[(Int,a)])
splitty m = splitty' m []

splitty' :: Int -> [(Int,a)] -> [(Int,a)] -> ([(Int,a)],[(Int,a)])
splitty' _ as [] = (as, [])
splitty' m as ((n, a) : cs) | m < n = (as, (n,a) : cs)
splitty' m as ((n, a) : cs) = splitty' (m - n) ((n,a):as) cs

randomS :: (Random a,MonadState StdGen m) => (a, a) -> m a
randomS bds = do
  (a, r') <- randomR bds <$> get
  put r'
  return a

