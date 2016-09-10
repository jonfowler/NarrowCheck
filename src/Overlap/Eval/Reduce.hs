module Overlap.Eval.Reduce where

import Overlap.Eval.Expr
import Overlap.Eval.Env
import Overlap.Eval.Monad
import Overlap.Lens

import Data.Maybe
import Data.List hiding (nub)

import Overlap.Printer

import Debug.Trace

import qualified Data.IntMap as I

data Susp' a b = Fin a 
               | Susp [FId] b 

type Susp = Susp' Expr Expr

--reduceTrace :: Monad m => [Expr] -> Expr ->  OverlapT m Susp
--reduceTrace as e = do
--  env <- get
--  trace (printDoc (printState (if null as then e else App e as) env)) $ reduce reduceTrace as e 

           
reduce :: Monad m => Maybe Int -> [Expr] -> Expr ->  OverlapT m Susp

reduce cx as (Let v e e') = bind v e >> reduce cx as e'

reduce cx as (App e es) = reduce cx (es ++ as) e 

reduce cx as (Fun f) = do
  r <- inlineDef as f
  case r of
    Nothing -> return . Fin $ App (Fun f) as
    Just (as', e) -> reduce cx as' e

reduce cx as (Var v) = do
  e <- use (env . at' v)
  a <- reduce cx [] e
  case a of
    Fin e' -> do
      env . at v ?= e'
      reduce cx as e' 
--    Susp x (Var v') -> do
--      env . at v ?= Var v'
--      return $ Susp x (if null as then Var v' else App (Var v') as) 
    Susp x e' -> do 
      env . at v ?= e'
      return $ Susp x (if null as then Var v else App (Var v) as) 

reduce cx [] (FVar x) = do
  a <- use (free . at x)
  case a of
    Just (cid, fids) -> return . Fin $ Con cid (FVar <$> fids) 
    Nothing -> return $ Susp [x] (FVar x) 

reduce cx [] (Lam v e) = return . Fin $ Lam v e

reduce cx (a : as) (Lam v e) = bind v a >> reduce cx as e

reduce cx _ Bottom = return . Fin $ Bottom

reduce cx [] (Con cid es) = return . Fin $ Con cid es

reduce cx as (Local fid vm d) = do
  a <- reduceLocal cx fid vm d 
  case a of
    Fin e -> reduce cx as e
    Susp x (vm', d') -> return $ Susp x (App (Local fid vm' d') as)

reduce _ as (Con cid es) = return . Fin $ Con cid (es ++ as) 

type LMap = I.IntMap Expr

toSuspMap :: [Int] -> I.IntMap ()
toSuspMap = I.fromList . flip zip (repeat ())

reduceLocal :: Monad m => Maybe XId ->
                        FId -> LMap -> Def -> OverlapT m (Susp' Expr (LMap, Def))
reduceLocal cx fid vm (Result vs e) = do
  i <- use nextLVar
  f <- bindVars vm vs 
  return . Fin $ replaceExpr f e
reduceLocal cx fid vm (Match i sxs alts solxs ol) = do
  let e = fromMaybe (error "reduceLocal variable not in map") $ I.lookup i vm  
  case I.null sxs || maybe True (flip I.member sxs) cx of
    True -> do
      a <- reduce cx [] e 
      case a of
        Fin (Con cid es) ->  do
          (vs, d) <- matcher fid cid alts 
          reduceLocal cx fid (I.union (I.fromList $ zip vs es) vm) d 
        Susp x e' -> reduceOverlap (I.insert i e' vm) x (toSuspMap x) 
    False -> reduceOverlap vm (I.keys sxs) sxs

--      maybe
--       (return $ Susp (I.keys sxs ++ I.keys solxs) (vm,
--                         Match i sxs alts solxs ol))
--       reduceOverlap
--       (if I.null solxs || I.member cx solxs then ol else Nothing)
--             where reduceOverlap d = do
--                     r <- reduceLocal cx fid vm d
--                     case r of
--                       Fin e'' -> return . Fin $ e''
--                       Susp x' (vm', d') -> return $
--                               Susp (I.keys sxs ++ x') (vm',
--                                   Match i sxs alts (toSuspMap x') (Just d'))

  where
    reduceOverlap :: Monad m => LMap -> [XId] -> I.IntMap () -> OverlapT m (Susp' Expr (LMap, Def))
    reduceOverlap vm x sxs = maybe 
             (return $ Susp (x ++ nub' sxs (I.keys solxs)) (vm, Match i sxs alts solxs ol))
             reduceOverlap' 
             (if I.null solxs || maybe True (flip I.member solxs) cx then ol else Nothing)
      where
        reduceOverlap' d = do
           r <- reduceLocal cx fid vm d
           case r of
             Fin e -> return . Fin $ e
             Susp x' (vm', d') -> return $ Susp (x ++ nub' sxs x') (vm', Match i sxs alts (toSuspMap x') (Just d'))

nub :: [Int] -> [Int]
nub = nub' I.empty

nub' _ [] = []
nub' s (x : xs) = if I.member x s then nub' s xs else x : nub' (I.insert x () s) xs

matcher :: Monad m => FId -> CId -> [Alt] -> OverlapT m ([Int], Def)
matcher fid cid (Alt cid' is d : as) | cid == cid' = return (is, d)
matcher fid cid (AltDef d : _) = return  ([], d)
matcher fid cid (_ : as) = matcher fid cid as
matcher fid cid [] = do 
  f <- use (funcNames . at' fid)
  error $ "Incomplete pattern in function: " ++ f

inlineDef :: Monad m => [Expr] -> FId -> OverlapT m (Maybe ([Expr], Expr))
inlineDef es f = do
  (vs, d) <- use (defs . at' f)
  if length es < length vs
   then return Nothing
   else let (as, es') = splitAt (length vs) es
            vm = I.fromList (zip vs as) 
        in return . Just $ (es', Local f vm d)

binds :: Monad m => [LId] -> [Expr] -> OverlapT m ()
binds vs es = mapM_ (uncurry bind) (zip vs es)




bind :: Monad m => LId -> Expr -> OverlapT m ()
bind v e = env . at v ?= e

bindVars :: Monad m => LMap -> [Int] -> OverlapT m (Int -> Int)
bindVars vm [] = return (\x -> error "variable wasn't bound")
bindVars vm (e : es) = do
  f1 <- bindVar vm e
  f <- bindVars vm es
  return (\x -> case f1 x of
            Just n -> n
            Nothing -> f x)

bindVar :: Monad m => LMap -> Int -> OverlapT m (Int -> Maybe Int)
bindVar vm b = do
  v <- use nextLVar
  case I.lookup b vm of
    Nothing -> return ()
    Just e -> bind v e
  nextLVar += 1
  return (\x -> if x == b then Just v else Nothing)
  

replaceExpr :: (Int -> Int) -> Expr -> Expr
replaceExpr f (Let v e e') = Let (f v) (replaceExpr f e) (replaceExpr f e')
replaceExpr f (App e e') = App (replaceExpr f e) (replaceExpr f <$> e') 
replaceExpr f (Fun fid) = Fun fid
replaceExpr f (Var v) = Var $ f v
replaceExpr f (FVar x) = FVar x
replaceExpr f (Lam v e) = Lam (f v) $ replaceExpr f e
replaceExpr f Bottom = Bottom
replaceExpr f (Con cid es) = Con cid (replaceExpr f <$> es)
replaceExpr f (Local fid i d) = Local fid i d


                            

--replaceAtom :: Int -> Atom -> Atom 
--replaceAtom v (Fun f) = Fun f
--replaceAtom v (Var x) = Var (v + x)
--replaceAtom v (Lam x e) = Lam (v + x) (replaceExpr v e)
--replaceAtom v (FVar x) = FVar x
--replaceAtom v Bottom = Bottom
--replaceAtom v (Con c as) = Con c (map (replaceAtom v) as)

                        
