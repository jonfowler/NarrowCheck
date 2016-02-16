module Reach.Eval.Reduce where

import Reach.Eval.Expr
import Reach.Eval.Monad
import Reach.Eval.Gen
import Reach.Eval.Env
import Reach.Lens
import Reach.Printer
import Data.List
import Data.IntMap (IntMap)
import qualified Data.IntMap as I

import Debug.Trace

type Reduce m = Atom -> [Conts] -> ReachT m Susp 
type Expr' = (Atom, [Conts])

data Susp = Susp FId [Conts]
          | SuspL LId [Conts]
          | Fin Atom deriving Show

reduceTrace :: Monad m => Reduce m -> Reduce m
reduceTrace r e cs = do
  s <- get
  trace (printDoc (printState (Expr e cs) s)) $ reduce r e cs


reduce :: Monad m => Reduce m -> Reduce m
reduce r (Lam x e)    [] = return . Fin $ Lam x e
reduce r (Con cid es) [] = return . Fin $ Con cid es
reduce r (Lam x e') (Apply e : cs) = do
  bind x e
  (e'', cs') <- bindLets e'
  r e'' (cs' ++ cs)
reduce r (Con cid es) (Branch a as : cs) = do
  (e, cs') <- match cid es as >>= bindLets
  r e (cs' ++ cs)
reduce r (Con cid es) (Apply e : cs) = fail "Partial constructors not implemented yet"
reduce r (Fun fid) cs = do
  e <- use (funcs . at' fid) >>= inlineFunc 
  (e', cs') <- bindLets e
  r e' (cs' ++ cs) 
reduce r (Var ev) cs = do
  (e, cs') <- use (env . at' ev) >>= bindLets
  s <- r e cs'
  case s of
    Susp x cs'' -> do
      env . at ev ?= Expr (FVar x) cs''
      return (Susp x (cs'' ++ cs))
    SuspL v cs'' -> do
      env . at ev ?= Expr (Var v) cs''
      return (SuspL v (cs'' ++ cs))
    Fin a -> do
      env . at ev ?= Expr a [] 
      r a cs
reduce r (FVar x) cs = do
  c <- use (free . at x)
  case c of
    Just (cid, fids) -> r (Con cid (map FVar fids)) cs
    Nothing -> return $ Susp x cs

inlineFunc :: Monad m => Func -> ReachT m Expr
inlineFunc (Func e vs) = do 
  i <- use nextEVar
  nextEVar += vs
  return (replaceExpr i e) 

match :: Monad m => CId -> [Atom] -> [Alt Expr] -> ReachT m Expr
match  cid es (Alt cid' xs c : as)
  | cid == cid' = binds xs (map atom es) >> return c
  | otherwise   = match cid es as
match  cid es (AltDef e : _) = return e
match _ _ [] = error "REACH_ERROR: no match for constructor in case"

                         

binds :: Monad m => [LId] -> [Expr] -> ReachT m ()
binds vs es = mapM_ (uncurry bind) (zip vs es)

bind :: Monad m => LId -> Expr -> ReachT m ()
bind v e = env . at v ?= e

bindLets :: Monad m => Expr -> ReachT m Expr'
bindLets (Let x e e') = do
  e'' <- bind x e
  bindLets e'
bindLets (Expr a cs) = return (a, cs)

replaceExpr :: Int -> Expr -> Expr
replaceExpr v (Let x e e') = Let (x + v) (replaceExpr v e) (replaceExpr v e')
replaceExpr v (Expr e cs) = Expr (replaceAtom v e) (map replaceConts cs)
   where
     replaceConts (Apply e) = Apply (replaceExpr v e)
     replaceConts (Branch a as) = Branch (replaceExpr v a) (map replaceAlt as)

     replaceAlt (Alt c vs e) = Alt c (map (v+) vs) (replaceExpr v e)
     replaceAlt (AltDef e) = AltDef (replaceExpr v e)

replaceAtom :: Int -> Atom -> Atom
replaceAtom v (Fun f) = Fun f
replaceAtom v (Var x) = Var (v + x)
replaceAtom v (Lam x e) = Lam (v + x) (replaceExpr v e)
replaceAtom v (FVar x) = FVar x
replaceAtom v (Con c as) = Con c (map (replaceAtom v) as)

