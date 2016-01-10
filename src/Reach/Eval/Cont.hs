module Reach.Eval.Cont where

import Reach.Eval.Expr
import Reach.Eval.Monad
import Reach.Eval.Gen
import Reach.Eval.Env
import Reach.Lens
import Reach.Printer
import Debug.Trace
import Data.List

runReach :: Monad m => ReachT m a -> Env -> m (Either ReachFail (a , Env))
runReach m s = runExceptT (runStateT m s)

type Reduce m = Atom -> [Conts] -> ReachT m Expr' 
type Expr' = (Atom, [Conts])
type Susp = (FId, [Conts])

evalLazy :: MonadChoice m => Atom -> [Conts] -> ReachT m Atom
evalLazy e conts = do
   c <- fix reduce e conts
   case c of 
     (Lam v e, []) -> do
        x <- newFVar
        evalLazy (Lam v e) [Apply . atom . FVar $ x] 
     (e, []) -> return e
     (FVar x, Branch as : cs) -> do
       (cid,vs) <- choose x (map (\(Alt c vs e) -> (c,length vs)) as)
       xs <- fvars x vs
       free . at x ?= (cid, xs)
       evalLazy (Con cid (map FVar xs)) (Branch as : cs)

choose :: MonadChoice m => FId -> [(CId, Int)] -> ReachT m (CId, Int)
choose _ = foldr ((<|>) . return) memp 

evalBase :: MonadChoice m => Expr' -> ReachT m Atom 
evalBase e = do
  r <- evalInter e
  case r of
    Left (FVar x, Branch as : cs) -> do
       (cid,vs) <- choose x (map (\(Alt c vs e) -> (c,length vs)) as)
       xs <- fvars x vs
       free . at x ?= (cid, xs)
       evalBase (Con cid (map FVar xs), Branch as : cs)
    Right (Lam v e, []) -> do
      x <- newFVar
      evalBase (Lam v e, [Apply . atom . FVar $ x])
    Right (a, ss) -> return a
    o -> error (show o)

evalInter :: Monad m => Expr' -> ReachT m (Either Expr' (Atom, [Susp]) )
evalInter (e, cs) = do
  c <- fix reduce e cs
  case c of
    (LVar v, cs') -> return (Left (LVar v, cs'))
    (FVar x, cs') -> do 
      i <- interweaver cs' 
      case i of
        Left cs'' -> return . Left $ (FVar x, cs'')
--        Right ((cid, as), cs'', ss) -> do
--          let es = fmap (Expr (FVar x) . return . Branch) (transposeSemiAtom as)
--          xs <- evars es 
--          s <- get
--          trace (printDoc (printState (Expr e cs) s)) (return . Right $ (Con cid (map EVar xs), (x, cs'') : ss))
    (Con c es, []) -> return $ Right (Con c es, [])
    (Lam x e, []) -> return $ Right (Lam x e, [])


addCont :: Conts -> Either [Conts] (SemiAtom, [Conts], [Susp]) ->
                     Either [Conts] (SemiAtom, [Conts], [Susp]) 
addCont c (Left cs) = Left (c : cs)
addCont c (Right (e, cs', es)) = Right (e, cs', es)

type SemiAtom = (CId, [Alt [Atom]])

transposeSemiAtom :: [Alt [Atom]] -> [[Alt Expr]]
transposeSemiAtom = transpose . map (fmap (fmap (flip Expr [])) . sequence)
       

interweaver :: Monad m => [Conts] -> ReachT m (Either [Conts] (SemiAtom, [Conts], [Susp]))
interweaver [] = return (Left [])
interweaver (Apply e : cs) = addCont (Apply e) <$> interweaver cs
interweaver (Branch as : cs) = do
  i <- interweave as  
  case i of 
    Left as' -> addCont (Branch as') <$> interweaver cs
--    Right (e, es) -> return (Right (e, cs , es))

interweave :: Monad m => [Alt Expr] -> ReachT m (Either [Alt Expr] (SemiAtom, [Susp]))
interweave as = do
  as' <- (mapM . mapM) (bindLets >=> evalInter) as
  return (Left ((fmap . fmap) (either (uncurry Expr) (flip Expr [] . fst)) as'))
--  case consol' as' of 
--    Left as'' -> return (Left as'')
--    Right as'' -> trace "blah" $ case consolidate as'' of
--      Nothing -> return . Left $ (fmap . fmap) (flip Expr [] . fst) as''
--      Just e -> trace "blah2" $ return . Right $ e
  
consol' :: [Alt (Either Expr' (Atom, [Susp]))] -> Either [Alt Expr] [Alt (Atom, [Susp])]
consol' [] = Right []
consol' (Alt cid vs e : as) = case consol' as of
  Left as -> Left (Alt cid vs (either (uncurry Expr) (flip Expr [] . fst) e) : as)
  Right as -> case e of
    Left e -> Left (Alt cid vs (uncurry Expr e) : (fmap . fmap) (flip Expr [] . fst) as)
    Right e -> Right (Alt cid vs e : as)


consolidate :: [Alt (Atom, [Susp])] -> Maybe (SemiAtom, [Susp])
consolidate [Alt c vs (Con cid es, s)] = Just ((cid, [Alt c vs es]), s) 
consolidate (Alt c vs (Con cid es, s) : as) = do
   ((cid', ars), s') <- consolidate as
   if cid' == cid
     then return ((cid', Alt c vs es : ars), s ++ s')
     else Nothing

           
reduceTrace :: Monad m => Reduce m -> Reduce m
reduceTrace r e cs = do
  s <- get
  trace (printDoc (printState (Expr e cs) s)) $ reduce r e cs

bindLets :: Monad m => Expr -> ReachT m Expr'
bindLets (Let x e e') = do
  e'' <- bind x e e'  
  bindLets e''
bindLets (Expr a cs) = return (a, cs)
  
reduce :: Monad m => Reduce m -> Reduce m
reduce r (Lam x e)    [] = return (Lam x e, [])
reduce r (Con cid es) [] = return (Con cid es, [])
reduce r (Lam x e') (Apply (Expr a []) : cs) = do
  e'' <- replaceLVar x a e'
  (e , cs') <- bindLets e''
  r e (cs' ++ cs) 
reduce r (Lam x e') (Apply e : cs) = do
  (e'', cs') <- bind x e e' >>= bindLets 
  r e'' (cs' ++ cs)
reduce r (Con cid es) (Branch as : cs) = do
  (e, cs') <- match cid es as >>= bindLets
  r e (cs' ++ cs)
reduce r (Con cid es) (Apply e : cs) = fail "Partial constructors not implemented yet"
reduce r (Fun fid) cs = do
  Expr e cs' <- use (funcs . at' fid . body) 
  r e (cs' ++ cs) 
reduce r (EVar x) cs = do
  (e , cs') <- use (env . at' x) >>= bindLets
  (e' , cs'') <- r e cs'
  env . at x ?= Expr e' cs''
  -- TODO The behaviour here loses sharing..
  r e' (cs'' ++ cs)
reduce r (FVar x) cs = do
  c <- use (free . at x)
  case c of
    Just (cid, fids) -> r (Con cid (map FVar fids)) cs
    Nothing -> return (FVar x, cs)
reduce r (LVar x) cs = return (LVar x, cs)

match :: Monad m => CId -> [Atom] -> [Alt Expr] -> ReachT m Expr
match  cid es (Alt cid' xs c : as)
  | cid == cid' = replaceLVars xs es c  
  | otherwise   = match cid es as
match _ _ [] = error "REACH_ERROR: no match for constructor in case"
                         

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
  replaceLVar x (EVar ex) c

evar :: Monad m => Expr -> ReachT m EId
evar e = do
  ex <- use nextEVar
  nextEVar += 1
  env . at ex ?= e
  return ex

evars :: Monad m => [Expr] -> ReachT m [EId]
evars = mapM evar 

replaceLVars :: Monad m => [LId] -> [Atom] -> Expr -> ReachT m Expr
replaceLVars [] [] e = return e
replaceLVars (v : vs) (e : es) e' = replaceLVar v e e' >>= replaceLVars vs es

replaceLVar :: Monad m => LId -> Atom -> Expr -> ReachT m Expr
replaceLVar v a (Let x e e')
  | x == v    = return $ Let x e e'
  | otherwise = Let x <$> (replaceLVar v a e) <*> (replaceLVar v a e')
replaceLVar v a (Expr e cs) = Expr <$> (replaceAtom v a e) <*> (mapM replaceConts cs)
   where
     replaceConts (Apply e) = Apply <$> replaceLVar v a e
     replaceConts (Branch as) = Branch <$> mapM replaceAlt as

     replaceAlt (Alt c vs e)
       | v `elem` vs  = return $ Alt c vs e
       | otherwise    = Alt c vs <$> replaceLVar v a e

replaceAtom :: Monad m => LId -> Atom -> Atom -> ReachT m Atom
replaceAtom v a (Fun f) = return $ Fun f
replaceAtom v a (EVar x) = do
  e <- use (env . at' x)
  e' <- replaceLVar v a e
  env . at x ?= e'
  return $ EVar x
replaceAtom v a (LVar v')
  | v == v'   = return a 
  | otherwise = return $ LVar v' 
replaceAtom v a (Lam x e)
  | v == x    = return $ Lam x e
  | otherwise = Lam x <$> (replaceLVar v a e)
replaceAtom v a (FVar x) = return $ FVar x
replaceAtom v a (Con c as) = Con c <$> (mapM (replaceAtom v a) as)

newFVars :: Monad m => Int -> ReachT m [FId]
newFVars n = replicateM n newFVar

newFVar :: Monad m => ReachT m FId
newFVar = do
  x <- use nextFVar
  nextFVar += 1
  freeDepth . at x ?= 0
  topFrees %= (x :)
  return x

fvar :: Monad m => FId -> ReachT m FId
fvar xo = do
  x <- use nextFVar
  nextFVar += 1

  maxd <- use maxDepth
  d <- use (freeDepth . at' xo)

  if d < maxd 
     then do
       freeDepth . at x ?= d + 1
       return x
     else throwError DataLimitFail

fvars :: Monad m => FId -> Int -> ReachT m [FId]
fvars xo n = replicateM n (fvar xo)

