module Overlap.Eval.Narrow where

import Overlap.Eval.Expr
import Overlap.Eval.Env
import Overlap.Eval.Reduce
import Overlap.Eval.Monad

import Overlap.Lens

import Overlap.Printer

import qualified Data.Map as M
import qualified Data.IntMap as I
import Data.IntMap (IntMap)
import Data.Maybe

import Debug.Trace

runOverlap :: Monad m => OverlapT m a -> Env Expr -> m (Either OverlapFail a , Env Expr)
runOverlap m = runStateT (runExceptT m)

narrowSetup :: Monad m => String -> OverlapT m Expr
narrowSetup fname = do
  fid' <- use (funcIds . at fname)
  case fid' of
    Nothing -> error $ "The " ++ fname ++ " function does not a type"
    Just fid -> do
      ts <- use (defArgTypes . at' fid)
      xs <- mapM (fvar 0) ts
      topFrees .= xs
      return $ App (Fun fid) (FVar <$> xs)

sizedSetup :: Monad m => Int -> String -> IntMap (OverlapT m Expr)
sizedSetup n fname = I.fromList $ map ss [0..(n-1)]
  where
  ss t = (t, do
    fid' <- use (funcIds . at fname)
    case fid' of
      Nothing -> error "The reach function does not a type"
      Just fid -> do
        (_ : ts) <- use (defArgTypes . at' fid)
        ev <- get 
        xs <- mapM (fvar 0) ts
        topFrees .= xs
        return $ App (Fun fid) (intToNat ev t : map FVar xs)) 

intToNat :: Env Expr -> Int -> Expr
intToNat ev 0 = Con zer []
  where zer = fromMaybe (error "Zero not defined") (M.lookup "Z" (ev ^. constrIds))
intToNat ev n = Con su [intToNat ev (n-1)]
  where su = fromMaybe (error "Succesor not defined") (M.lookup "S" (ev ^. constrIds))

narrow :: MonadChoice m => Maybe Int -> Expr -> OverlapT m Expr
narrow cx e = do
   c <- reduce cx [] e
   case c of 
     Fin a -> return a
     Susp (x : _) e -> do
       (cid, xs) <- choose x
       narrow (Just x) e

choose :: MonadChoice m => XId -> OverlapT m (CId, [FId])
choose x = do
  d <- use (freeDepth . at' x)
  maxd <- use maxDepth
  c <- use freeCount
  freeCount += 1
  maxc <- use maxFreeCount
  when (maxc < c) (throwError DataLimitFail)
  when (maxd <= d) (throwError DataLimitFail)
  t <- use (freeType . at' x)
  as <- use (typeConstr . at' t)
  (cid, ts) <-  mchoice (map (\(cid, n, ts) -> (n, pure (cid, ts))) as)

--  ev <- get
--  topxs <- use (topFrees)
  xs <- --trace (printXVars topxs ev) $
        mapM (fvar (d + 1)) ts
  free . at x ?= (cid, xs)
  env <- get
  return (cid,xs)

fvar :: Monad m => Int -> Type -> OverlapT m FId
fvar d t = do
  x <- use nextFVar
  nextFVar += 1
  freeDepth . at x ?= d
  freeType . at x ?= t
  return x

 

