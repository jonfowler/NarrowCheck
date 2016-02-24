module Reach.Eval.Reduce where

import Reach.Eval.Expr
import Reach.Eval.Monad
import Reach.Eval.Env
import Reach.Lens
import Reach.Printer
import Data.List
import Data.IntMap (IntMap)
import qualified Data.IntMap as I

import Debug.Trace

type FullAlts = [(Expr', [Alts])]

type Reduce m = Expr -> [Expr] -> FullAlts -> ReachT m Susp
type FullReduce m = FId -> Expr -> [Expr] -> FullAlts -> ReachT m Susp 
--type Expr' = (Atom, [Conts])
--
data Susp = Susp FId Expr'
          | Fin Atom deriving Show


--reduceTrace :: Monad m => Reduce m -> Reduce m -> Reduce m
--reduceTrace s r e = do
--  env <- get
--  trace (printDoc (printState e env)) $ reduce s r e 


reduce :: Monad m => FullReduce m -> Reduce m -> Reduce m
reduce s r (Let x e e') ap br = do
  bind x (toExpr' e)
  r e' ap br
reduce s r (Lam x e) [] [] = return . Fin $ Lam x e
reduce s r (Lam x e') (e : es) bs = do
  bind x (toExpr' e)
  r e' es bs
reduce s r (App e e') ap br = reduce s r e (e' : ap) br

reduce s r (Con cid es) [] [] = return . Fin $ Con cid es
reduce s r Bottom _ _ = return $ Fin Bottom
reduce s r (Con cid es) [] ((_, [as]) : br) = do
  e <- match cid (toExpr' <$> es) as
  r e [] br
reduce s r (Con cid es) [] ((e, as : ass) : br) = do
  e <- match cid (toExpr' <$> es) as
  r e [] ((toExpr' e, ass) : br)
reduce s r (Fun fid) ap br = do
 e <- use (funcs . at' fid) >>= inlineFunc 
 r e ap br
reduce s r (Case e Bottom as) [] ((e', ass): br) = reduce s r e [] ((e', as : ass) : br)
reduce s r (Case e e' as) [] br = reduce s r e [] ((toExpr' e', [as])  : br)

--  a <- reduce s r e
--  case a of
--    Fin c -> r (Case c e' as)
--    Susp x e -> do
--        a <- s e'
--        case a of
--          Fin Bottom -> return $ Susp x (Case e Bottom as)
--          Fin a -> return $ Fin a
--          Susp x' e' -> return $ Susp x (Case e e' as)
--      return $ Susp x (Case e e' as)

reduce s r (Var v) ap br = do
  Expr' e ap' br' <- use (env . at' v)
  a <- r e ap' br'
  case a of
    Fin a -> do
      env . at v ?= toExpr' a
      r a ap br
    Susp x e -> do
      env . at v ?= e
      s x (Var v) ap br
reduce s r (FVar x) ap br = do
  a <- use (free . at x)
  case a of
    Just (cid, fids) -> r (Con cid (FVar <$> fids)) ap br
    Nothing -> s x (FVar x) ap br

--reduce r (Con cid es) (Branch a as : cs) = do
--  (e, cs') <- match cid es as >>= bindLets
--  r e (cs' ++ cs)
--reduce r (Con cid es) (Apply e : cs) = fail "Partial constructors not implemented yet"


--reduce r (Var ev) cs = do
--  (e, cs') <- use (env . at' ev) >>= bindLets
--  s <- r e cs'
--  case s of
--    Susp x cs'' -> do
--      env . at ev ?= Expr (FVar x) cs''
--      return (Susp x (cs'' ++ cs))
--    SuspL v cs'' -> do
--      env . at ev ?= Expr (Var v) cs''
--      return (SuspL v (cs'' ++ cs))
--    Fin a -> do
--      env . at ev ?= Expr a [] 
--      r a cs
--reduce r (FVar x) cs = do
--  c <- use (free . at x)
--  case c of
--    Just (cid, fids) -> r (Con cid (map FVar fids)) cs
--    Nothing -> return $ Susp x cs

inlineFunc :: Monad m => Func -> ReachT m Expr
inlineFunc (Func e vs) = do 
  i <- use nextEVar
  nextEVar += vs
  return (replaceExpr i e) 

match :: Monad m => CId -> [Expr'] -> [Alt Expr] -> ReachT m Expr
match  cid es (Alt cid' xs c : as)
  | cid == cid' = binds xs es >> return c
  | otherwise   = match cid es as
match  cid es (AltDef e : _) = return e
match _ _ [] = return Bottom -- error "REACH_ERROR: no match for constructor in case"

                         

binds :: Monad m => [LId] -> [Expr'] -> ReachT m ()
binds vs es = mapM_ (uncurry bind) (zip vs es)

bind :: Monad m => LId -> Expr' -> ReachT m ()
bind v e = env . at v ?= e

--bindLets :: Monad m => Expr -> ReachT m Expr'
--bindLets (Let x e e') = do
--  e'' <- bind x e
--  bindLets e'
--bindLets (Expr a cs) = return (a, cs)

--replaceExpr :: Int -> Expr -> Expr
--replaceExpr v (Let x e e') = Let (x + v) (replaceExpr v e) (replaceExpr v e')
--replaceExpr v (Expr e cs) = Expr (replaceAtom v e) (map replaceConts cs)
--   where
--     replaceConts (Apply e) = Apply (replaceExpr v e)
--     replaceConts (Branch a as) = Branch (replaceExpr v a) (map replaceAlt as)
--
replaceExpr :: Int -> Expr -> Expr
replaceExpr v (Let x e e') = Let (v + x) (replaceExpr v e) (replaceExpr v e')
replaceExpr v (Fun f) = Fun f
replaceExpr v (Var x) = Var (v + x)
replaceExpr v (Lam x e) = Lam (v + x) (replaceExpr v e)
replaceExpr v (FVar x) = FVar x
replaceExpr v Bottom = Bottom
replaceExpr v (Case e e' as) = Case (replaceExpr v e)
                                    (replaceExpr v e')
                                    (map replaceAlt as)
  where 
     replaceAlt (Alt c vs e) = Alt c (map (v+) vs) (replaceExpr v e)
     replaceAlt (AltDef e) = AltDef (replaceExpr v e)
replaceExpr v (App e e') = App (replaceExpr v e) (replaceExpr v e')
replaceExpr v (Con c as) = Con c (map (replaceExpr v) as)

