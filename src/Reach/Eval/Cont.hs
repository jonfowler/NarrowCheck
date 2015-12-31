module Reach.Eval.Cont where

import Reach.Eval.Expr
import Reach.Eval.Monad
import Reach.Eval.Gen
import Reach.Eval.Env
import Reach.Lens
import Reach.Printer
import Debug.Trace

runReach :: Monad m => ReachT m a -> Env -> m (a , Env)
runReach m s = do
  r <- runExceptT (runStateT m s)
  case r of
    Left err -> fail . show $ err
    Right a -> return a

type Reduce m = Atom -> [Conts] -> ReachT m (Atom, [Conts])

evalLazy :: MonadChoice m => Atom -> [Conts] -> ReachT m Atom
evalLazy e conts = do
   c <- fix reduce e conts
   case c of 
     (e, []) -> return e
     (FVar x, Branch as : cs) -> do
       (cid,vs) <- choose x (map (\(Alt c vs e) -> (c,length vs)) as)
       xs <- newFVars vs
       free . at x ?= (cid, xs)
       evalLazy (Con cid (map FVar xs)) (Branch as : cs)

choose :: MonadChoice m => FId -> [(CId, Int)] -> ReachT m (CId, Int)
choose _ = foldr ((<|>) . return) memp 

           
reduceTrace :: Monad m => Reduce m -> Reduce m
reduceTrace r e cs = do
  s <- get
  trace (printDoc (printState (Expr e cs) s)) $ reduce r e cs

bindLets :: Monad m => Expr -> ReachT m (Atom, [Conts]) 
bindLets (Let x e e') = do
  e'' <- bind x e e'  
  bindLets e''
bindLets (Expr a cs) = return (a, cs)
  
reduce :: Monad m => Reduce m -> Reduce m
reduce r (Lam x e)    [] = return (Lam x e, [])
reduce r (Con cid es) [] = return (Con cid es, [])
reduce r (Lam x e') (Apply e : cs) = do
  (e'', cs') <- bind x e e' >>= bindLets 
  r e'' (cs' ++ cs)
reduce r (Con cid es) (Branch as : cs) = do
  (e, cs') <- bindLets $ match cid es as
  r e (cs' ++ cs)
reduce r (Con cid es) (Apply e : cs) = fail "Partial constructors not implemented yet"
reduce r (Fun fid) cs = do
  Expr e cs' <- use (funcs . at' fid . body) 
  r e (cs' ++ cs) 
reduce r (EVar x) cs = do
  (e , cs') <- use (env . at' x) >>= bindLets
  (e' , cs'') <- r e cs'
  env . at x ?= Expr e' cs''
  r e' (cs'' ++ cs)
reduce r (FVar x) cs = do
  c <- use (free . at x)
  case c of
    Just (cid, fids) -> r (Con cid (map FVar fids)) cs
    Nothing -> return (FVar x, cs)

match ::  CId -> [Atom] -> [Alt Expr] -> Expr
match  cid es (Alt cid' xs c : as)
  | cid == cid' = replaceLVars xs es c  
  | otherwise   = match cid es as
match _ _ [] = error "no match for constructor in case"
                         

binds :: Monad m => [LId] -> [Expr] -> Expr -> ReachT m Expr 
binds (x : xs) (e : es) e' = bind x e e' >>= binds xs es
binds [] [] e' = return e'
binds _ _ _ = error "Constructor / Alterenative variable mismatch"

-- Bind x to e in e', A new environment variable, ex, is created for x and the
-- variable x is replaced with ex in e'. Then ex is bound to e in the environment.
bind :: Monad m => LId -> Expr -> Expr -> ReachT m Expr 
bind x e c = do
  ex <- use nextEVar
  nextEVar += 1
  env . at ex ?= e
  return (replaceLVar x (EVar ex) c)

replaceLVars :: [LId] -> [Atom] -> Expr -> Expr
replaceLVars [] [] e = e
replaceLVars (v : vs) (e : es) e' = replaceLVar v e (replaceLVars vs es e')

replaceLVar :: LId -> Atom -> Expr -> Expr
replaceLVar v a (Let x e e')
  | x == v    = Let x e e'
  | otherwise = Let x (replaceLVar v a e) (replaceLVar v a e')
replaceLVar v a (Expr e cs) = Expr (replaceAtom v a e) (map replaceConts cs)
   where
     replaceConts (Apply e) = Apply (replaceLVar v a e)
     replaceConts (Branch as) = Branch $ map replaceAlt as

     replaceAlt (Alt c vs e)
       | v `elem` vs  = Alt c vs e
       | otherwise    = Alt c vs (replaceLVar v a e)

replaceAtom :: LId -> Atom -> Atom -> Atom
replaceAtom v a (Fun f) = Fun f
replaceAtom v a (EVar x) = EVar x
replaceAtom v a (LVar v')
  | v == v'   = a 
  | otherwise = LVar v' 
replaceAtom v a (Lam x e)
  | v == x    = Lam x e
  | otherwise = Lam x (replaceLVar v a e)
replaceAtom v a (FVar x) = FVar x
replaceAtom v a (Con c as) = Con c (map (replaceAtom v a) as)
--                               Cont (replaceLVarExpr v e e')
--                                    (map replaceConts as)
--  where replaceConts (Branch as) = Branch $ (fmap . fmap) (replaceLVar v e) as
--        replaceConts (Apply e') = Apply $ replaceLVarExpr v e e'

--replaceLVarExpr :: LId -> Expr -> Expr -> Expr
--replaceLVarExpr lx ex (Let x e e') = Let x (replaceLVarExpr lx ex e) (replaceLVarExpr lx ex e')
--replaceLVarExpr lx ex (Fun f) = Fun f
--replaceLVarExpr lx ex (EVar x) = EVar x
--replaceLVarExpr lx ex (LVar lx')
--  | lx == lx' = ex
--  | otherwise = LVar lx'
--replaceLVarExpr lx ex (App e e') = App (replaceLVarExpr lx ex e) (replaceLVarExpr lx ex e')
--replaceLVarExpr lx ex (Lam x e)
--  | x == lx = Lam x e
--  | otherwise = Lam x (replaceLVarExpr lx ex e)
--replaceLVarExpr lx ex (Case e as) = Case (replaceLVarExpr lx ex e) (map replaceAlt as)
--    where replaceAlt (Alt cid xs e') = Alt cid xs (replaceLVarExpr lx ex e')
--replaceLVarExpr lx ex (Con cid es) = Con cid (fmap (replaceLVarExpr lx ex) es)
--replaceLVarExpr lx e (FVar x) = FVar x 

newFVars :: Monad m => Int -> StateT Env m [FId]
newFVars n = replicateM n newFVar

newFVar :: Monad m => StateT Env m FId
newFVar = do
  x <- use nextFVar
  nextFVar += 1
  return x




