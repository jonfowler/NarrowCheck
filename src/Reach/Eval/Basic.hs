module Reach.Eval.Basic where

import Reach.Eval.Env
import Reach.Eval.Expr
import Reach.Eval.Monad
import Control.Lens hiding (snoc)
import qualified Data.DList as D 

runReach :: Monad m => ReachT Identity a -> Env -> m (a , Env)
runReach m s = case runExcept (runStateT m s) of
  Left err -> fail . show $ err
  Right a -> return a
  

       
eval :: Expr -> ReachT Identity Expr
eval (Let x e e')  = do
  a <- use (env . at x)
  case a of
    Just _ -> error "variable already bound"
    Nothing -> do
      env . at x ?= e 
      eval e'

eval (Fun fid) = inlineFunc fid

eval (Var x) = do
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
    Lam x e' -> do
      env . at x ?= e
      eval e'
    Con cid es -> return (Con cid (D.snoc es e))
    _ -> error "function evaluated to non lambda"
eval (Case e as) = do
  v <- eval e
  e' <- match v as
  eval e'
eval v = return v

match :: Monad m => Expr -> [Alt] -> ReachT m Expr
match (Con cid es) (Alt cid' xs e : as)
  | cid == cid' = binds xs (D.toList es) >> return e 
  | otherwise   = match (Con cid es) as
match _ [] = error "no match for constructor in case"
match _ _ = error "case subject did not evaluate to constructor"

binds :: Monad m => [LId] -> [Expr] -> ReachT m ()
binds (x : xs) (e : es) = bind x e >> binds xs es
binds [] [] = return ()
binds _ _ = error "Constructor / Alterenative variable mismatch"

bind :: Monad m => LId -> Expr -> ReachT m ()
bind x e = do
  a <- use (env . at x)
  case a of
    Just _ -> error "Variable already bound"
    Nothing -> env . at x ?= e

--             data Expr
--  = Let !LId Expr Expr
--  | Fun {-# UNPACK #-} !FId
--  | Var !LId
--  | App Expr Expr 
--  | Case Expr [Alt]
--  | Lam !LId Expr
--  | Con !CId (DList Expr) 





--evalApp :: Moand m => Expr -> [Expr] -> ReachT m Expr
       


inlineFunc :: Monad m => FId -> ReachT m Expr
inlineFunc fid = do
  Just f <- use (funcs.at fid)
  i <- use nextVar
  nextVar .= i + f ^. vars 
  return ((f ^. body) +<< i)

(+<<) :: Expr -> Int -> Expr 
Let x e e' +<< i = Let (x + i) (e +<< i) (e' +<< i)
Fun f +<< _ = Fun f
Var x +<< i = Var (x + i)
App e e' +<< i = App (e +<< i) (e' +<< i)  
Lam x e +<< i = Lam (x + i) (e +<< i)
Case e as +<< i = Case (e +<< i) (map incAlt as)
    where incAlt (Alt cid xs e') = Alt cid (map (+i) xs) e'
Con cid es +<< i = Con cid (fmap (+<< i) es)

(+@<<) :: Alt -> Int -> Expr 
a +@<< i = undefined

--data Func =
--  Func {_body :: Expr,
--        _vars :: Int
--       }

 



--eval 
