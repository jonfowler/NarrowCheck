module Reach.Eval.Cont where

import Reach.Eval.Expr
import Reach.Eval.Monad
import Reach.Eval.Gen
import Reach.Eval.Env
import Reach.Lens
import Debug.Trace

runReach :: Monad m => ReachT m a -> Env -> m (a , Env)
runReach m s = do
  r <- runExceptT (runStateT m s)
  case r of
    Left err -> fail . show $ err
    Right a -> return a

evalLazy :: MonadChoice m => Expr -> Conts -> ReachT m Expr
evalLazy e conts = do
   c <- reduce e conts
   case trace (show c) c of 
     Cont e Fin -> return e
     Cont (FVar x) (as :<: cs) -> do
       (cid,vs) <- choose x (map (\(Alt c vs e) -> (c,length vs)) as)
       xs <- newFVars vs
       free . at x ?= (cid, xs)
       evalLazy (Con cid (map FVar xs)) (as :<: cs)

choose :: MonadChoice m => FId -> [(CId, Int)] -> ReachT m (CId, Int)
choose _ = foldr ((<|>) . return) memp 
 

reduce :: Monad m => Expr -> Conts -> ReachT m Cont

reduce (Lam x e) Fin = return $ Cont (Lam x e) Fin
reduce (Con cid es) Fin = return $ Cont (Con cid es) Fin
                          
reduce (Lam x e') (e :$: conts) = trace "lam" $ do
  e'' <- bind x e e'
  trace (show e'') $ trace (show conts) $ reduce e'' conts

reduce (Con cid es) (as :<: conts) = trace "match" $
  let Cont e cs = match cid es as
  in reduce e (cs +++ conts)

reduce (Case e as) conts = trace "case"
  reduce e (fmap (fmap toCont) as :<: conts)

reduce (Let x e e') conts = trace "let" $ do
  e'' <- bind x e e'  
  reduce e'' conts

reduce (Fun fid) conts = trace "fun" $ do
  e <- use (funcs . at' fid . body) 
  reduce e conts

reduce (EVar x) conts = trace "var" $ trace (show conts) $ do
  Cont e cs <-  use (env . at' x)
  Cont e' cs' <- reduce e cs 
  env . at x ?= Cont e' cs' 
  trace "varfin" $ return (Cont e' (cs' +++ conts)) 

reduce (App f e) conts = trace "app" $ trace (show f) $ trace (show conts) $ reduce f (e :$: conts)

reduce (FVar x) conts = trace "fvar" $ do
  c <- use (free . at x)
  case c of
    Just (cid, fids) -> reduce (Con cid (map FVar fids)) conts
    Nothing -> return (Cont (FVar x) conts)

reduce (LVar x) conts = return (Cont (LVar x) conts)

reduce e cs = error ("Unexpected case in reduce: \n"++show e ++"\n" ++ show cs)


match ::  CId -> [Expr] -> [Alt Cont] -> Cont
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
  env . at ex ?= toCont e
  return (trace (show c) $ replaceLVarExpr x (EVar ex) c)

replaceLVars :: [LId] -> [Expr] -> Cont -> Cont
replaceLVars [] [] e = e
replaceLVars (v : vs) (e : es) e' = replaceLVar v e (replaceLVars vs es e')

replaceLVar :: LId -> Expr -> Cont -> Cont
replaceLVar v e (Cont e' as) = Cont (replaceLVarExpr v e e')
                                    (contsMap (replaceLVar v e) (replaceLVarExpr v e) as)
  
replaceLVarExpr :: LId -> Expr -> Expr -> Expr
replaceLVarExpr lx ex (Let x e e') = Let x (replaceLVarExpr lx ex e) (replaceLVarExpr lx ex e')
replaceLVarExpr lx ex (Fun f) = Fun f
replaceLVarExpr lx ex (EVar x) = EVar x
replaceLVarExpr lx ex (LVar lx')
  | lx == lx' = ex
  | otherwise = LVar lx'
replaceLVarExpr lx ex (App e e') = App (replaceLVarExpr lx ex e) (replaceLVarExpr lx ex e')
replaceLVarExpr lx ex (Lam x e)
  | x == lx = Lam x e
  | otherwise = Lam x (replaceLVarExpr lx ex e)
replaceLVarExpr lx ex (Case e as) = Case (replaceLVarExpr lx ex e) (map replaceAlt as)
    where replaceAlt (Alt cid xs e') = Alt cid xs (replaceLVarExpr lx ex e')
replaceLVarExpr lx ex (Con cid es) = Con cid (fmap (replaceLVarExpr lx ex) es)
replaceLVarExpr lx e (FVar x) = FVar x 

newFVars :: Monad m => Int -> StateT Env m [FId]
newFVars n = replicateM n newFVar

newFVar :: Monad m => StateT Env m FId
newFVar = do
  x <- use nextFVar
  nextFVar += 1
  return x




