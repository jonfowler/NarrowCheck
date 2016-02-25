module Reach.Eval.Diagonal where

import Reach.Eval.Monad
import System.Random

diag :: Tree a -> [a]
diag t = [x | Leaf x <- diagonal (levels [t])]

diagonal :: [[a]] -> [a]
diagonal = concat . foldr diags []
     where diags [] ys = ys
           diags (x : xs) ys = [x] : merge xs ys

           merge [] ys = ys
           merge xs [] = map (:[]) xs
           merge (x : xs) (y : ys) = (x : y) : merge xs ys


levels :: [Tree a] -> [[Tree a]]
levels ts | null ts = []
          | otherwise = ts : levels [ u | Branch us <- ts, u <- us]

shuffleTree :: StdGen -> Tree a -> Tree a
shuffleTree _ (Leaf a) = Leaf a
shuffleTree rnd (Branch ts) = Branch (shuffle r (length ts) (zipWith shuffleTree rs ts))
  where r:rs = splits rnd

splits :: StdGen -> [StdGen]
splits r = r' : splits r''
  where (r',r'') = split r

shuffle :: StdGen -> Int -> [a] -> [a]
shuffle rnd l vs = v : shuffle rnd' (l-1) (xs ++ xs')
  where (n, rnd') = randomR (0,l-1) rnd
        (xs, v : xs') = splitAt n vs

testTree :: Int -> Tree [Int]
testTree n = Branch (Leaf [] : map (\i -> (i :) <$> testTree n) [1..n])
