module Overlap.Eval.Enumerate where

import Overlap.Eval.Monad
import Overlap.Eval.Env
import Overlap.Eval.Expr
import Data.Maybe
import Control.Arrow

enumerate :: Tree a -> [a]
enumerate (Leaf a) = [a]
  --let p = f a in (maybeToList p, 0, if isNothing p then 1 else 0)
enumerate (Branch ts) = concatMap (enumerate . snd) ts
 -- let
 --        tester = if null res then 1 + cnt else cnt
 --        tester2 = sum . map trd3 $ enms
 --        in seq tester (seq tester2 (res, tester ,tester2))
 --  where enms = map (enumerate f . snd) ts
 --        res = concatMap fst3 enms
 --        cnt = sum (map snd3 enms)

counter l = counter' l (0,0)

counter' :: [Maybe a] -> (Int,Int) -> (Int, Int) 
counter' [] r = r
counter' (Just _ : l) (r0,r1) = let r0' = r0 + 1 in seq r0' (counter' l (r0', r1))
counter' (Nothing : l) (r0,r1) = let r1' = r1 + 1 in seq r1' (counter' l (r0, r1'))

--  foldl2 (\(a1,a2) b -> maybe (a1, a2+1) (const (a1+1,a2)) b) (0,0)


enumSearchTree :: OverlapTree a -> Env Expr -> [(Either OverlapFail a, Env Expr)]
enumSearchTree s e = undefined

enumST :: MapTree a -> Env Expr -> (a, Maybe (MapTree a))
enumST (Leaf (s,f)) e = undefined





fst3 (x,_,_) = x
snd3 (_,y,_) = y
trd3 (_,_,z) = z
