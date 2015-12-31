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

type Reduce m = Atom -> [Conts] -> ReachT m Expr' 
type Expr' = (Atom, [Conts])
type Susp = (FId, [Conts])

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


evalInter :: Monad m => Expr' -> ReachT m (Either Expr' (Atom, [Susp]) )
evalInter (e, cs) = do
  c <- fix reduce e cs
  case c of
    (LVar v, cs') -> return (Left (LVar v, cs'))
    (FVar x, cs') -> do
      i <- interweaver cs'
      return $ case i of
        Left cs'' -> Left (FVar x, cs'')
        Right (e', cs'', es) -> Right (e', (x, cs'') : es)
    (Con c es, []) -> return $ Right (Con c es, [])
    (Lam x e, []) -> return $ Right (Lam x e, [])


addCont :: Conts -> Either [Conts] (Atom, [Conts], [Susp]) ->
                     Either [Conts] (Atom, [Conts], [Susp]) 
addCont c (Left cs) = Left (c : cs)
addCont c (Right (e, cs', es)) = Right (e, c : cs', es)

interweaver :: Monad m => [Conts] -> ReachT m (Either [Conts] (Atom , [Conts] , [Susp]))
interweaver (Apply e : cs) = addCont (Apply e) <$> interweaver cs
interweaver (Branch as : cs) = do
  i <- interweave as  
  case i of 
    Left as' -> addCont (Branch as') <$> interweaver cs
    Right (e, es) -> return (Right (e, [], es))

interweave :: [Alt Expr] -> ReachT m (Either [Alt Expr] (Atom , [Susp]))
interweave = undefined

consolidate :: [Alt (Atom, [Susp])] -> Maybe (CId, [Alt [Atom]], [Susp])
consolidate [Alt c vs (Con cid es, s)] = Just (cid, [Alt c vs es], s) 
consolidate (Alt c vs (Con cid es, s) : as) = do
   (cid', ars, s') <- consolidate as
   if cid' == cid
     then return (cid', Alt c vs es : ars, s ++ s')
     else Nothing
     
           
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
reduce r (LVar x) cs = return (LVar x, cs)

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




