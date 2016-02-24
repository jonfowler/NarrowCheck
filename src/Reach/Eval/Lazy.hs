module Reach.Eval.Lazy where

import Reach.Eval.Reduce
import Reach.Eval.Expr
import Reach.Eval.Monad
import Reach.Eval.Env
import Reach.Lens
import Reach.Printer
import Data.List
import Data.IntMap (IntMap)
import qualified Data.IntMap as I

import Debug.Trace       

runReach :: Monad m => ReachT m a -> Env -> m (Either ReachFail (a , Env))
runReach m s = runExceptT (runStateT m s)

evalSetup :: Monad m => String -> ReachT m Expr
evalSetup fname = do
  fid' <- use (funcIds . at fname)
  case fid' of
    Nothing -> error "The reach function does not a type"
    Just fid -> do
      fexpr <- use (funcs . at' fid) >>= inlineFunc 
      ts <- use (funcArgTypes . at' fid)
      xs <- mapM (fvar 0) ts
      topFrees .= xs
      return (foldr (\x e -> App e (FVar x)) fexpr (reverse xs))

evalLazy :: MonadChoice m => Expr -> [Expr] -> FullAlts -> ReachT m Atom
evalLazy e ap br = do
   c <- fix (reduce (\x e ap br -> return $ Susp x (Expr' e ap br))) e ap br
   case c of 
     Fin a -> return a
     Susp x (Expr' e ap br) -> do
       (cid, xs) <- choose x
       evalLazy e ap br

reduceFull' :: Monad m => FullAlts  -> ReachT m (Either FullAlts Susp)
reduceFull' [] = return . Left $ [] 
reduceFull' ((Expr' e' ap' br', ass) : br) = do
  a <- fix (reduce reduceFull) e' ap' br'
  case a of
    Fin Bottom -> case br of
      [] -> return . Left $ [(Expr' Bottom [] [], ass)]
      ((z, ass') : br) -> reduceFull' ((z, ass ++ ass') : br)
    Fin a -> Right <$> fix (reduce reduceFull) a [] br
    Susp _ z -> do
      a <- reduceFull' br
      case a of
        Left brs -> return . Left $ ((z, ass) : brs)
        Right s -> return $ Right s

reduceFull :: Monad m => FullReduce m 
reduceFull x e ap br = do
  a <- reduceFull' br
  case a of
    Left br' -> return . Susp x $ Expr' e ap br'
    Right s  -> return s
--reduceFull x e ap ( 
      
    

evalFull :: MonadChoice m => Expr -> [Expr] -> FullAlts -> ReachT m Atom
evalFull e ap br = do
   c <- fix (reduce reduceFull) e ap br
   case c of 
     Fin a -> return a
     Susp x (Expr' e ap br) -> do
       (cid, xs) <- choose x
       evalFull e ap br

choose :: MonadChoice m => FId -> ReachT m (CId, [FId])
choose x = do
  d <- use (freeDepth . at' x)
  maxd <- use maxDepth
  when (maxd <= d) (throwError DataLimitFail)
  t <- use (freeType . at' x)
  as <- use (typeConstr . at' t)
  (cid, ts) <- mchoice (map pure as)
  xs <- mapM (fvar (d + 1)) ts
  free . at x ?= (cid, xs)
  return (cid, xs)

fvar :: Monad m => Int -> Type -> ReachT m FId
fvar d t = do
  x <- use nextFVar
  nextFVar += 1
  freeDepth . at x ?= d
  freeType . at x ?= t
  return x

suspToExpr :: Susp -> Expr'
suspToExpr (Fin a) = Expr' a [] []
suspToExpr (Susp x e) = e 

evar :: Monad m => Expr' -> ReachT m EId
evar e = do
  ex <- use nextEVar
  nextEVar += 1
  env . at ex ?= e
  return ex

evars :: Monad m => [Expr'] -> ReachT m [EId]
evars = mapM evar 

newFVars :: Monad m => Int -> ReachT m [FId]
newFVars n = replicateM n newFVar

newFVar :: Monad m => ReachT m FId
newFVar = do
  x <- use nextFVar
  nextFVar += 1
  freeDepth . at x ?= 1
  topFrees %= (x :)
  return x



--scopeExpr :: Monad m => Expr -> ReachT m (IntMap ())
--scopeExpr (Let v e e') = I.union <$> scopeExpr e <*> (I.delete v <$> (scopeExpr e'))
--scopeExpr (Expr a cs) =  I.union <$> scopeAtom a
--                                 <*> foldM (\a b -> I.union a <$> (scopeConts b)) I.empty cs
--
--scopeAtom :: Monad m => Atom -> ReachT m (IntMap ())
--scopeAtom (Fun _) = return $ I.empty
--scopeAtom (Var v) = use  (env . at' v) >>= scopeExpr
--
--scopeAtom (LVar v) = return $ I.singleton v () 
--scopeAtom (FVar _) = return $ I.empty
--scopeAtom (Lam v e) = I.delete v <$> scopeExpr e
--scopeAtom (Con c as) = I.unions <$> mapM scopeAtom as 
--
--scopeConts :: Monad m => Conts -> ReachT m (IntMap ())
--scopeConts (Branch _ as) = I.unions <$> mapM scopeAlt as
--    where scopeAlt (Alt _ vs e) = I.difference
--                                  <$> scopeExpr e
--                                  <*> pure (I.fromList $ zip vs (repeat ())) 
--          scopeAlt (AltDef e) = scopeExpr e
--scopeConts (Apply e) = scopeExpr e 
--
--scopingExpr :: Monad m => Expr -> ReachT m (IntMap ())
--scopingExpr (Let v e e') = I.union
--                           <$> scopingExpr e
--                           <*> (I.insert v () <$> scopingExpr e')
--scopingExpr (Expr a cs) =  I.union <$> scopingAtom a
--                                 <*> foldM (\a b -> I.union a <$> (scopingConts b)) I.empty cs
--
--scopingAtom :: Monad m => Atom -> ReachT m (IntMap ())
--scopingAtom (Fun _) = return $ I.empty
--scopingAtom (EVar v) = use  (env . at' v) >>= scopingExpr
--
--scopingAtom (LVar _) = return $ I.empty
--scopingAtom (FVar _) = return $ I.empty
--scopingAtom (Lam v e) = I.insert v () <$> scopingExpr e
--scopingAtom (Con c as) = I.unions <$> mapM scopingAtom as 
--
--scopingConts :: Monad m => Conts -> ReachT m (IntMap ())
--scopingConts (Branch _ as) = I.unions <$> mapM scopingAlt as
--    where scopingAlt (Alt _ vs e) = I.union 
--                                  <$> scopingExpr e
--                                  <*> pure (I.fromList $ zip vs (repeat ())) 
--          scopingAlt (AltDef e) = scopingExpr e
--scopingConts (Apply e) = scopingExpr e 
