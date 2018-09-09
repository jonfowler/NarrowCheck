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

--runOverlap :: Monad m => OverlapT m a -> Env Expr -> m (Either OverlapFail a , Env Expr)
--runOverlap m = runStateT (runExceptT m)

narrowSetup :: Monad m => String -> OverlapT m Expr
narrowSetup fname = do
          (fid, ts) <- getFunc fname
          e <- use typeConstr
          xs <- mapM (fvar 0 . narrowSet e) ts
          topFrees .= xs
          return $ App (Fun fid) (FVar <$> xs)

sizedSetup :: Monad m => Int -> String -> OverlapT m Expr
sizedSetup n fname = do
        (fid, Type t _ : ts) <- getFunc fname
        tid <- use (typeIds . at' "Nat")
        if tid /= t
          then error ("The first argument of " ++ fname ++ " should have type Nat"
                   ++ " when using the sized setting")
          else do
            ev <- get
            e <- use typeConstr
            xs <- mapM (fvar 0 . narrowSet e) ts
            topFrees .= xs
            return $ App (Fun fid) (intToNat ev n : map FVar xs)

narrowSet :: IntMap [(CId, Int, [TypeExpr])] -> Type -> NarrowSet
narrowSet e (Type tid ts) = Narrow . map narrowCid $ e ^. at' tid
  where narrowCid (cid,n,tes) = (cid,n, map (narrowSet e . applyType ts) tes)

basicSetup :: MonadChoice m => Int -> String -> OverlapT m (Expr, [Expr])
basicSetup n fname = do
  (fid, ts) <- getFunc fname
  es <- mapM (generateType n) ts
  return (App (Fun fid) es, es)

generateType :: MonadChoice m => Int -> Type -> OverlapT m Expr
generateType n (Type tid ts) = do
  cs <- gofilter n <$> use (typeConstr . at' tid)
  if null cs then return Bottom
  else do
   (cid, tes)   <- mchoice (map (\(cid, fq, tes) -> (fq, pure (cid, tes))) cs)
   es <- mapM (generateType (n-1) . applyType ts) tes
   return (Con cid es)
     where gofilter n' | n' <= 0 = filter (\(_,_,tes) -> null tes)
           gofilter _ = id

--generateType n (Type tid ts) = do
--   cs <- use (typeConstr . at' tid)
--   mapM gogenerate cs
--   where gogenerate (cid, fq, )



getFunc :: Monad m => String -> OverlapT m (FId, [Type])
getFunc fname = do
  fid' <- use (funcIds . at fname)
  case fid' of
    Nothing -> error $ "The " ++ fname ++ " function is not defined"
    Just fid -> do
      ts' <- use (defArgTypes . at fid)
      case ts' of
        Nothing -> error $ "The " ++ fname
                             ++ " function does not have a type"
        Just ts -> return (fid,ts)


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
     Susp (x : _) e' -> do
       _ <- choose x
       narrow (Just x) e'
     _ -> error "narrow, Susp should have argument"

choose :: MonadChoice m => XId -> OverlapT m (CId, [FId])
choose x = do
  d <- use (freeDepth . at' x)
  maxd <- use maxDepth
  c <- use freeCount
  freeCount += 1
  maxc <- use maxFreeCount
  let t = maxd <= d || maxc < c
  as <- gofilter t <$> use (freeNarrowSet . at' x . getNarrowSet)
  when (null as) (throwError DataLimitFail)
  (cid, ts) <-  mchoice (map (\(cid, n, ts) -> (n, pure (cid, ts))) as)
  xs <- mapM (fvar (d + 1)) ts
  free . at x ?= (cid, xs)
  return (cid,xs)
    where gofilter True = filter (\(_,_,l) -> null l)
          gofilter False = id

chooseNew :: XId -> Overlap [(Int, (CId, [NarrowSet]))]
chooseNew x = do
  d <- use (freeDepth . at' x)
  maxd <- use maxDepth
  c <- use freeCount
  freeCount += 1
  maxc <- use maxFreeCount
  when (maxc < c) (throwError DataLimitFail)
  when (maxd <= d) (throwError DataLimitFail)
  as <- use (freeNarrowSet . at' x . getNarrowSet)
  return $ map (\(cid, n, ts) -> (n, (cid, ts))) as

applyFree :: XId -> (CId, [NarrowSet]) -> Overlap ()
applyFree x (cid, ts) = do
  d <- use (freeDepth . at' x)
  xs <- mapM (fvar (d + 1)) ts
  free . at x ?= (cid, xs)

narrowNew :: Maybe Int -> Expr -> OverlapTree Expr
narrowNew cx e = OverlapTree $ do
   c <- reduce cx [] e
   case c of
     Fin a -> return . Right $ a
     Susp (x : _) e' -> do
       cs <- chooseNew x
       return . Left $ (map . fmap) goNarr cs
         where goNarr r = OverlapTree $ do
                  applyFree x r
                  runOverlapTree $ narrowNew (Just x) e'



--narrowNew :: MonadChoice m => Maybe Int -> Expr -> OverlapT m Expr
--narrowNew = undefined
--


fvar :: Monad m => Int -> NarrowSet -> OverlapT m FId
fvar d t = do
  x <- use nextFVar
  nextFVar += 1
  freeDepth . at x ?= d
  freeNarrowSet . at x ?= t
  return x

 

