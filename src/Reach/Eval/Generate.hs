module Reach.Eval.Generate where

import Reach.Eval.Monad
import System.Random hiding (newStdGen)
import Reach.Eval.Expr
import Reach.Lens

newStdGen :: State StdGen StdGen
newStdGen = do
  (n, n') <- split <$> get
  put n
  return n'

generating :: Int -> (a -> Maybe b) -> Tree a -> State StdGen [b] 
generating bt p t = do
  (a, t') <- generate bt p t
  case a of
    Left _ -> generating bt p t'
    Right a -> (a :) <$> generating bt p t'
       
generate :: Int -> (a -> Maybe b) -> Tree a -> State StdGen (Either Int b, Tree a)
generate bt p (Leaf a) = case p a of
  Just a -> return $ (Right a, Branch [])
  Nothing -> return $ (Left bt, Branch [])
generate bt p (Branch []) = return $ (Left bt, Branch [])
generate bt p (Branch ts) = do
  (a, ts') <- select ts
  return (a, Branch ts')
  where --select :: [Tree a] -> State StdGen (Either Int b, [Tree a])
        select [] = return (Left $ 100000000, [])
        select ts = do
          (xs, t : xs')  <- flip splitAt ts <$> randomS (0, length ts - 1)
          (e, t) <- generate bt p t
          let ts' = if nullT t then (xs ++ xs') else t : xs ++ xs'
          case e of
            Left 0 -> return $ (Left 0, ts')
            Left i -> (_1 . _Left %~ min i) <$> select (xs ++ xs')
            Right a -> return $ (Right a, ts')
 
  
randomS :: Random a => (a, a) -> State StdGen a
randomS bds = do
  (a, r') <- randomR bds <$> get
  put r'
  return a

   

--(Left 0 : _) = Left 0
--select (Left i : ts) = _Left %~ min i $ select ts
--select (Right a : _) = Right a

--diag :: Tree a -> [a]
--diag t = [x | Leaf x <- diagonal (levels [t])]
--
--diagonal :: [[a]] -> [a]
--diagonal = concat . foldr diags []
--     where diags [] ys = ys
--           diags (x : xs) ys = [x] : merge xs ys
--
--           merge [] ys = ys
--           merge xs [] = map (:[]) xs
--           merge (x : xs) (y : ys) = (x : y) : merge xs ys
--
--
--levels :: [Tree a] -> [[Tree a]]
--levels ts | null ts = []
--          | otherwise = ts : levels [ u | Branch us <- ts, u <- us]
--
--shuffleTree :: StdGen -> Tree a -> Tree a
--shuffleTree _ (Leaf a) = Leaf a
--shuffleTree rnd (Branch ts) = Branch (shuffle r (length ts) (zipWith shuffleTree rs ts))
--  where r:rs = splits rnd

splits :: StdGen -> [StdGen]
splits r = r' : splits r''
  where (r',r'') = split r

shuffle :: StdGen -> Int -> [a] -> [a]
shuffle rnd l vs = v : shuffle rnd' (l-1) (xs ++ xs')
  where (n, rnd') = randomR (0,l-1) rnd
        (xs, v : xs') = splitAt n vs

testTree :: Int -> Tree [Int]
testTree n = Branch (Leaf [] : map (\i -> (i :) <$> testTree n) [1..n])
