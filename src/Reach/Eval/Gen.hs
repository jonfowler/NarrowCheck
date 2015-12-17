module Reach.Eval.Gen where

import Reach.Lens
import Reach.Eval.Env
import Reach.Eval.Expr
import Reach.Eval.Monad

{-
runReach :: Monad m => ReachT m a -> Env -> m (a , Env)
runReach m s = do
  r <- runExceptT (runStateT m s)
  case r of
    Left err -> fail . show $ err
    Right a -> return a

type Choose m = FId -> [(CId, Int)] -> ReachT m (CId, Int)

evalLazy :: MonadChoice m => Expr -> ReachT m Expr
evalLazy = evalGen chooseSimple
     
chooseSimple :: MonadChoice m => Choose m
chooseSimple _ as = foldr (<|>) memp (map return as)

deepEvalGen :: Monad m => Choose m -> Expr -> ReachT m Expr
deepEvalGen m e = do
  Con cid es <- evalGen m e
  env <- get
  vs <- mapM (deepEvalGen m) es
  return (Con cid vs)
       
evalGen :: Monad m => Choose m -> Expr -> ReachT m Expr
evalGen c (Let x e e')  = do
  a <- use (env . at x)
  case a of
    Just _ -> error "variable already bound"
    Nothing -> bind x e e' >>= evalGen c

evalGen c (Fun fid) = use (funcs . at' fid . body) >>= evalGen c

evalGen c (EVar x) = do
  a <- use (env . at x)
  case a of
     Nothing -> error "variable not bound"
     Just e -> do 
       v <- evalGen c e 
       env . at x ?= v
       return v

evalGen c (App f e) = do
  l <- evalGen c f
  case l of
    Lam x e' -> bind x e e' >>= evalGen c
    _ -> error "function evaluated to non lambda"

evalGen c (Case e as) = do
  v <- evalGen c e
  (cid',es') <- case v of 
    Con cid es -> return (cid, es)
    FVar x -> do
      (cid,vs) <- c x (map (\(Alt c vs e) -> (c,length vs)) as)
      xs <- newFVars vs
      free . at x ?= (cid, xs)
      return (cid, map FVar xs)
  evalGen c (match cid' es' as)

evalGen c (FVar x) = do
  c <- use (free . at x)
  case c of
    Just (cid, fids) -> return (Con cid (map FVar fids))
    Nothing -> return (FVar x)
evalGen _ v = return v

match ::  CId -> [Expr] -> [Alt] -> Expr
match  cid es (Alt cid' xs e : as)
  | cid == cid' = replaceLVars xs es e  
  | otherwise   = match cid es as
match _ _ [] = error "no match for constructor in case"

binds :: Monad m => [LId] -> [Expr] -> Expr -> ReachT m Expr
binds (x : xs) (e : es) e' = bind x e e' >>= binds xs es
binds [] [] e' = return e'
binds _ _ _ = error "Constructor / Alterenative variable mismatch"

-- Bind x to e in e', A new environment variable, ex, is created for x and the
-- variable x is replaced with ex in e'. Then ex is bound to e in the environment.
bind :: Monad m => LId -> Expr -> Expr -> ReachT m Expr
bind x e e' = do
  ex <- use nextEVar
  nextEVar += 1
  env . at ex ?= e
  return (replaceLVar x (EVar ex) e')

replaceLVars :: [LId] -> [Expr] -> Expr -> Expr 
replaceLVars [] [] e = e
replaceLVars (v : vs) (e : es) e' = replaceLVar v e (replaceLVars vs es e')
  
replaceLVar :: LId -> Expr -> Expr -> Expr 
replaceLVar lx ex (Let x e e') = Let x (replaceLVar lx ex e) (replaceLVar lx ex e')
replaceLVar lx ex (Fun f) = Fun f
replaceLVar lx ex (EVar x) = EVar x
replaceLVar lx ex (LVar lx')
  | lx == lx' = ex
  | otherwise = LVar lx'
replaceLVar lx ex (App e e') = App (replaceLVar lx ex e) (replaceLVar lx ex e')
replaceLVar lx ex (Lam x e) = Lam x (replaceLVar lx ex e)
replaceLVar lx ex (Case e as) = Case (replaceLVar lx ex e) (map replaceAlt as)
    where replaceAlt (Alt cid xs e') = Alt cid xs (replaceLVar lx ex e')
replaceLVar lx ex (Con cid es) = Con cid (fmap (replaceLVar lx ex) es)
replaceLVar lx e (FVar x) = FVar x 

newFVars :: Monad m => Int -> StateT Env m [FId]
newFVars n = sequence (replicate n newFVar)

newFVar :: Monad m => StateT Env m FId
newFVar = do
  x <- use nextFVar
  nextFVar += 1
  return x

-}
