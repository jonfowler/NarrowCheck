
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

taker :: Int -> [Maybe a] -> (Int, [a])
taker 0 _ = (0,[])
taker _ [] = (0,[])
taker n (Nothing : l) = let ~(i , l') = taker n l
                        in (i + 1, l')
taker n (Just a : l) = let ~(i , l') = taker (n -1) l
                       in (i, a : l')

generating :: Int -> Int -> (a -> Maybe b) -> Tree a -> StateT StdGen (Writer (Sum Int, Sum Int)) [b]
generating n bt p = generating' n bt . Just . fmap p

generating' :: Int -> Int -> Maybe (Tree (Maybe b)) -> StateT StdGen (Writer (Sum Int,Sum Int)) [b]
generating' _ _ Nothing = return []
generating' n _ _ | n <= 0 = return []
generating' _ _ (Just (Leaf b)) = return (maybeToList b)
generating' n bt (Just (Branch bs)) = do
   (b, t) <- brancher bt bs bt []
   case b of
     Nothing -> tell (mempty,Sum 1) >> generating' n bt (remake t)
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
                          ZipTree (Maybe b) -> StateT StdGen (Writer (Sum Int,Sum Int)) (Maybe b, ZipTree (Maybe b))
brancher _ [] i ts | i <= 1 || null ts = return (Nothing, ts)
brancher n [] i ((bs,_,bs') : ts) = tell (Sum 1,mempty) >> brancher n (bs ++ bs') (i -1) ts
brancher n bs i ts = do
  (xs, (fq, t) : xs') <- splitRand bs
  case t of
    Leaf (Just b) -> return (Just b, (xs, fq,  xs'):ts)
    Leaf Nothing | i == 0 -> return (Nothing, (xs, fq,  xs') : ts)
    Leaf Nothing -> brancher n (xs ++ xs') i ts
    Branch bs' -> brancher n bs' (min (i+1) n) ((xs, fq,  xs') : ts)

splitRand :: MonadState StdGen m => [(Int,a)] -> m ([(Int, a)], [(Int, a)])
splitRand l = do
  n <- randomS (0, tot - 1)
  return $ splitty n l
    where tot = sumOf (folded . _1) l

splitRandR :: MonadState StdGen m => [(a,Int,b)] -> m ([(a,Int, b)], [(a,Int, b)])
splitRandR l = do
  n <- randomS (0, tot - 1)
  return $ splittyR n l
    where tot = sumOf (folded . _2) l

splittyR :: Int -> [(a,Int,b)] -> ([(a,Int,b)],[(a,Int,b)])
splittyR _ [] = error "empty branch" 
splittyR m ((a,n, b) : cs) | m < n = ([], (a,n,b) : cs)
splittyR m (c@(_,n,_) : cs) = _1 %~ (c:) $ splittyR (m - n) cs

splitty :: Int -> [(Int,a)] -> ([(Int,a)],[(Int,a)])
splitty m = splitty' m []

splitty' :: Int -> [(Int,a)] -> [(Int,a)] -> ([(Int,a)],[(Int,a)])
splitty' _ as [] = error "empty branch"  -- (as, [])
splitty' m as ((n, a) : cs) | m < n = (as, (n,a) : cs)
splitty' m as ((n, a) : cs) = splitty' (m - n) ((n,a):as) cs

randomS :: (Random a,MonadState StdGen m) => (a, a) -> m a
randomS bds = do
  (a, r') <- randomR bds <$> get
  put r'
  return a


data RTree = REnd | RBranch [(Int,Int,RTree)]

type ZipR a = [([(Int,Tree a)] ,[(Int,Int,RTree)], (Int, Int), [(Int,Int,RTree)])]

remakeR :: ZipR a -> Maybe RTree
remakeR [] = Nothing
remakeR ((_,[],_,[]) : ts) = remakeR ts
remakeR ((_,bs,_,bs') : ts) = Just $ remakeR' (RBranch (bs ++ bs')) ts

remakeR' :: RTree -> ZipR a -> RTree
remakeR' t [] = t
remakeR' t ((_,bs,(i,fq),bs') : ts) = remakeR' (RBranch (bs ++ (i,fq,t) : bs')) ts


generatingR :: Int -> (e -> Tree (Maybe b)) -> e -> State StdGen [Maybe b]
generatingR bt f' e' = goGenR bt f' e' REnd

{-# NOINLINE goGenR #-}
goGenR bt f e rt = do
          (a, zt) <- genR bt (f e) bt [] rt
          case remakeR zt of
            Nothing -> return [a]
            Just rt' -> (a:) <$> goGenR bt f e rt'

noutTree :: Tree (Maybe b) -> Bool
noutTree (Leaf Nothing) = True
noutTree (Branch []) = True
noutTree _ = False

noutRTree :: RTree -> Bool
noutRTree (RBranch []) = True
noutRTree _ = False

rbranch :: [(Int, Tree a)] -> [(Int,Int,RTree)]
rbranch bs = foldr go (const []) bs 0
  where go (fq, _) f i = (i, fq, REnd) : f (i + 1)


--genR _ t 0 backtracks _ | noutTree t = return (Nothing, backtracks)
--genR bt t i ((bs,b1,_ ,b2) : backtracks) _ | noutTree t = genR bt (Branch bs) (i-1) backtracks (RBranch (b1++b2))

genR :: Int -> Tree (Maybe b) -> Int -> ZipR (Maybe b)-> RTree -> State StdGen (Maybe b, ZipR (Maybe b))
genR bt t p btks zt | noutTree t || noutRTree zt = case btks of
  _ | null btks || p == 0 -> return (Nothing, btks)
  ((bs,b1,_ ,b2) : btks') -> genR bt (Branch bs) (p-1) btks' (RBranch (b1 ++ b2))
genR _ (Leaf (Just a)) _ btks _ = return (Just a, btks)
genR bt (Branch bs) p btks rt = case rt of
    REnd -> go (rbranch bs)
    RBranch rbs -> go rbs
  where go rbs = do
         (rb1,(i,fq,rt') : rb2) <- splitRandR rbs
         let t = snd (bs !! i)
         genR bt t (min (p + 1) bt) ((bs, rb1, (i,fq), rb2) : btks) rt'
genR _ _ _ _ _ = error "genR incomplete"


