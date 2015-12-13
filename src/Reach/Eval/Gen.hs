module Reach.Eval.Gen where

import Reach.Lens
import Reach.Eval.Env
import Reach.Eval.Expr
import Reach.Eval.Monad

import qualified Data.DList as D 

runReach :: Monad m => ReachT m a -> Env -> m (a , Env)
runReach m s = do
  r <- runExceptT (runStateT m s)
  case r of
    Left err -> fail . show $ err
    Right a -> return a

type Match m = Expr -> [Alt] -> ReachT m Expr
  
deepEval :: Monad m => Expr -> ReachT m Expr
deepEval = deepEvalGen matchBasic

deepEvalGen :: Monad m => Match m -> Expr -> ReachT m Expr
deepEvalGen m e = do
  Con cid es <- evalGen m e
  env <- get
  vs <- mapM (deepEvalGen m) (D.toList es)
  return (Con cid (D.fromList vs))
       
evalGen :: Monad m => Match m -> Expr -> ReachT m Expr
evalGen m (Let x e e')  = do
  a <- use (env . at x)
  case a of
    Just _ -> error "variable already bound"
    Nothing -> bind x e e' >>= evalGen m

evalGen m (Fun fid) = use (funcs . at' fid . body) >>= evalGen m

evalGen m (EVar x) = do
  a <- use (env . at x)
  case a of
     Nothing -> error "variable not bound"
     Just e -> do 
       v <- evalGen m e 
       env . at x ?= v
       return v

evalGen m (App f e) = do
  l <- evalGen m f
  case l of
    Lam x e' -> bind x e e' >>= evalGen m
    Con cid es -> return (Con cid (D.snoc es e))
    _ -> error "function evaluated to non lambda"
evalGen m (Case e as) = do
  v <- evalGen m e
  e' <- m v as
  evalGen m e'
evalGen m (FVar x) = do
  c <- use (free . at x)
  case c of
    Just (cid, fids) -> return (Con cid (D.fromList $ map FVar fids))
    Nothing -> return (FVar x)
evalGen m v = return v

matchBasic :: Monad m => Match m
matchBasic (Con cid es) (Alt cid' xs e : as)
  | cid == cid' = binds xs (D.toList es) e
  | otherwise   = matchBasic (Con cid es) as
matchBasic _ [] = error "no match for constructor in case"
matchBasic e _ = error $ "case subject did not evaluate to constructor: " ++ show e

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
   
