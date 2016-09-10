module Overlap.Eval.Generate where

import Overlap.Eval.Monad
import System.Random hiding (newStdGen)
import Overlap.Eval.Expr
import Overlap.Lens

import qualified Data.IntMap as I
import Data.IntMap(IntMap)

import Data.Maybe

import Debug.Trace

newStdGen :: State StdGen StdGen
newStdGen = do
  (n, n') <- split <$> get
  put n
  return n'

generating :: Int -> (a -> State StdGen (Maybe b)) -> Tree a -> State StdGen [b] 
generating bt p t | nullT t = return []
generating bt p t = do
  (a, t') <- generate bt p t
  case a of
    Left _ -> generating bt p t'
    Right a -> (a :) <$> generating bt p t'

sizedGenerating :: Int -> Int -> (a -> State StdGen (Maybe b)) -> IntMap (Tree a) -> State StdGen [(Int, b)]
sizedGenerating n = sizedGenerating' [0..n-1]

sizedGenerating' :: [Int] -> Int -> (a -> State StdGen (Maybe b)) -> IntMap (Tree a) -> State StdGen [(Int, b)]
sizedGenerating' [] _ _ _ = return []
sizedGenerating' (cs : rs) bt p mt = if nullT t
                                          then sizedGenerating' rs bt p mt
                                          else do
  (a, t') <- generate bt p t 
  case a of
    Left _ -> sizedGenerating' (cs : rs) bt p (I.insert cs t' mt)
    Right a -> ((cs, a) :) <$>  sizedGenerating' (rs ++ [cs]) bt p (I.insert cs t' mt)
 where t = fromJust $ I.lookup cs mt
       
generate :: Int -> (a -> State StdGen (Maybe b)) -> Tree a -> State StdGen (Either Int b, Tree a)
generate bt p (Leaf a) = do
  a' <- p a 
  case a' of
    Just b -> return $ (Right b, Branch [])
    Nothing -> return $ (Left bt, Branch [])
generate bt p (Branch []) = return $ (Left bt, Branch [])
generate bt p (Branch ts) = do
  (a, ts') <- select bt ts
  return (a, Branch ts')
  where --select :: [Tree a] -> State StdGen (Either Int b, [Tree a])
        select i [] = 
          return (Left (i-1), [])
        select i ts = do
          (xs, (fq, t) : xs')  <- splitRand ts 
          (e, t) <- generate bt p t
          let ts' = if nullT t then (xs ++ xs') else xs ++ (fq, t) : xs'
          case e of
            Left 0 -> return $ (Left 0, ts')
            Left i' -> select (min i i') (xs ++ xs')
            Right a -> return $ (Right a, ts')

splitRand :: [(Int,a)] -> State StdGen ([(Int, a)], [(Int, a)]) 
splitRand l = do
  n <- randomS (0, tot - 1)
  return $ splitty n l 
    where tot = sumOf (folded . _1) l

splitty m cs = splitty' m [] cs

splitty' :: Int -> [(Int,a)] -> [(Int,a)] -> ([(Int,a)],[(Int,a)])
splitty' m as [] = (as, []) 
splitty' m as ((n, a) : cs) | m < n = (as, (n,a) : cs)
splitty' m as ((n, a) : cs) = splitty' (m - n) ((n,a):as) cs 
 
-- (_1 . _Left %~ min i) 
  
randomS :: Random a => (a, a) -> State StdGen a
randomS bds = do
  (a, r') <- randomR bds <$> get
  put r'
  return a

