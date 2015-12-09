module Reach.Eval.Basic where

import Reach.Lens
import Reach.Eval.Env
import Reach.Eval.Expr
import Reach.Eval.Monad

import Debug.Trace
import qualified Data.DList as D 

runReach :: Monad m => ReachT Identity a -> Env -> m (a , Env)
runReach m s = case runExcept (runStateT m s) of
  Left err -> fail . show $ err
  Right a -> return a
  

deepEval :: Expr -> ReachT Identity Expr
deepEval e = do
  Con cid es <- eval e
  env <- get
  vs <- --trace (show es) . trace (show env) $
          mapM deepEval (D.toList es)
  return (Con cid (D.fromList vs))
       
eval :: Expr -> ReachT Identity Expr
eval (Let x e e')  = do
  a <- use (env . at x)
  case a of
    Just _ -> error "variable already bound"
    Nothing -> bind x e e' >>= eval 

eval (Fun fid) = do
  f <- use (funcs . at' fid . body)
  trace (show f) $ eval f

eval (EVar x) = do
  a <- use (env . at x)
  case a of
     Nothing -> error "variable not bound"
     Just e -> do 
       v <- eval e 
       env . at x ?= v
       return v

eval (App f e) = do
  l <- eval f
  case l of
    Lam x e' -> bind x e e' >>= eval
    Con cid es -> return (Con cid (D.snoc es e))
    _ -> error "function evaluated to non lambda"
eval (Case e as) = do
  v <- trace (show $ Case e as) $ eval e
  e' <- match v as
  eval e'
eval v = return v

match :: Monad m => Expr -> [Alt] -> ReachT m Expr
match (Con cid es) (Alt cid' xs e : as)
  | cid == cid' = binds xs (D.toList es) e
  | otherwise   = match (Con cid es) as
match _ [] = error "no match for constructor in case"
match e _ = error $ "case subject did not evaluate to constructor: " ++ show e

binds :: Monad m => [LId] -> [Expr] -> Expr -> ReachT m Expr
binds (x : xs) (e : es) e' = bind x e e' >>= binds xs es
binds [] [] e' = return e'
binds _ _ _ = error "Constructor / Alterenative variable mismatch"

-- Bind x to e in e', A new environment variable, ex, is created for x and the
-- variable x is replaced with ex in e'. Then ex is bound to e in the environment.
bind :: Monad m => LId -> Expr -> Expr -> ReachT m Expr
bind x e e' = do
  ex <- use nextVar
  nextVar += 1
  env . at ex ?= e
  return (replaceLVar x ex e')
  


--inlineFunc :: Monad m => FId -> ReachT m Expr
--inlineFunc fid = do
--  Just f <- use (funcs.at fid)
--  i <- trace (show f) $ use nextVar
--  nextVar .= trace2 (i + f ^. vars + 5)
--  return ((f ^. body) +<< i)

trace2 :: Show a => a -> a
trace2 a = trace (show a) a


replaceLVar :: LId -> EId -> Expr -> Expr 
replaceLVar lx ex (Let x e e') = Let x (replaceLVar lx ex e) (replaceLVar lx ex e')
replaceLVar lx ex (Fun f) = Fun f
replaceLVar lx ex (EVar x) = EVar x
replaceLVar lx ex (LVar lx')
  | lx == lx' = EVar ex
  | otherwise = LVar lx'
replaceLVar lx ex (App e e') = App (replaceLVar lx ex e) (replaceLVar lx ex e')
replaceLVar lx ex (Lam x e) = Lam x (replaceLVar lx ex e)
replaceLVar lx ex (Case e as) = Case (replaceLVar lx ex e) (map replaceAlt as)
    where replaceAlt (Alt cid xs e') = Alt cid xs (replaceLVar lx ex e')
replaceLVar lx ex (Con cid es) = Con cid (fmap (replaceLVar lx ex) es)
   
--Let x e e' +<< i = Let (x + i) (e +<< i) (e' +<< i)
--Fun f +<< _ = Fun f
--Var x +<< i = Var (x + i)
--App e e' +<< i = App (e +<< i) (e' +<< i)  
--Lam x e +<< i = Lam (x + i) (e +<< i)
--Case e as +<< i = Case (e +<< i) (map incAlt as)
--    where incAlt (Alt cid xs e') = Alt cid (map (+i) xs) (e' +<< i)
--Con cid es +<< i = Con cid (fmap (+<< i) es)


--data Func =
--  Func {_body :: Expr,
--        _vars :: Int
--       }

 



--eval 
