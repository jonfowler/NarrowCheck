module Reach.Eval.Cont where

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

runReach :: Monad m => ReachT m a -> Env -> m (Either ReachFail (a , Env))
runReach m s = runExceptT (runStateT m s)

type Reduce m = Atom -> [Conts] -> ReachT m Susp 
type Expr' = (Atom, [Conts])
--type Susp = (FId, [Conts])

data Susp = Susp FId [Conts]
          | SuspL LId [Conts]
          | Fin Atom deriving Show

evalSetup :: Monad m => String -> ReachT m Expr'
evalSetup fname = do
  fid' <- use (funcIds . at fname)
  case fid' of
    Nothing -> error "The reach function does not a type"
    Just fid -> do
      fexpr <- use (funcs . at' fid) >>= inlineFunc 
      ts <- use (funcArgTypes . at' fid)
      xs <- mapM (fvar 0) ts
      topFrees .= xs
      (e, cs) <- bindLets fexpr
      return (foldr (\x (e', cs') -> (e, Apply (atom $ FVar x) : cs')) (e, cs) xs)


evalLazy :: MonadChoice m => Expr' -> ReachT m Atom
evalLazy (e, conts) = do
   c <- fix reduce e conts
   case c of 
     Fin (Lam v e) -> do
        x <- newFVar
        evalLazy (Lam v e, [Apply . atom . FVar $ x])
     Fin a -> return a
     Susp x (Branch as : cs) -> do
--       (cid,vs) <- choose x (map (\(Alt c vs e) -> (c,length vs)) as)
--       xs <- fvars x vs
--       free . at x ?= (cid, xs)
       (cid, xs) <- choose x
       evalLazy (Con cid (map FVar xs), Branch as : cs)
     SuspL v _ -> error "Should not be suspended on local variable in evalLazy"

--choose :: MonadChoice m => FId -> [(CId, Int)] -> ReachT m (CId, Int)
--choose _ = foldr ((<|>) . return) memp 

choose :: MonadChoice m => FId -> ReachT m (CId, [FId])
choose x = do
  d <- use (freeDepth . at' x)
  maxd <- use maxDepth
  when (maxd <= d) (throwError DataLimitFail)
  t <- use (freeType . at' x)
  as <- use (typeConstr . at' t)
  (cid, ts) <- mchoice (map pure as)
  xs <- mapM (fvar (d + 1)) ts
  free . at x ?= (cid, xs)
  return (cid, xs)

fvar :: Monad m => Int -> Type -> ReachT m FId
fvar d t = do
  x <- use nextFVar
  nextFVar += 1
  freeDepth . at x ?= d
  freeType . at x ?= t
  return x


--fvars :: Monad m => FId -> (ConId, [Type]) -> ReachT m FId
--fvars x cid t = do

--  d <- use (freeDepth . at' xo)
--  maxd <- use maxDepth
--  if d < maxd 
--     then do
--       x <- use nextFVar
--       nextFVar += 1
--       freeDepth . at x ?= d + 1
--       return x
--     else throwError DataLimitFail
--
--fvars :: Monad m => FId -> Int -> ReachT m [FId]
--fvars xo n = replicateM n (fvar xo)

evalBase :: MonadChoice m => Expr' -> ReachT m Atom 
evalBase e = do
  r <- evalInter e
  case r of
    Fin (Lam v e) -> do
      x <- newFVar
      evalBase (Lam v e, [Apply . atom . FVar $ x])
    Fin a -> return a
    Susp x (Branch as : cs) -> do
--       traceExpr' (FVar x, Branch as : cs)
--       (cid,vs) <- choose x (map (\(Alt c vs e) -> (c,length vs)) as)
--       xs <- fvars x vs
--       free . at x ?= (cid, xs)
       (cid, xs) <- choose x
       evalBase (Con cid (map FVar xs), Branch as : cs)
--       evalBase (Con cid (map FVar xs), Branch True as : cs)
--        Right (Con cid vs, []) -> return $ Con cid vs
    o -> error (show o)

evalInter :: Monad m => Expr' -> ReachT m Susp
evalInter (e, cs) = do
  c <- fix reduce e cs
  case c of
    SuspL v cs' -> return $ SuspL v cs'
    Susp x cs' -> do 
      i <- interweaver cs' 
      case i of
        Left cs'' -> return $ Susp x cs''
        Right (bcs, (cid, as), cs'') -> do
          subj <- evar (Expr (FVar x) bcs)
          let es = fmap (Expr (Var subj) . return . Branch) (transposeSemiAtom as)
          xs <- evars es 
--          traceExpr (Expr (Con cid (map EVar xs)) cs'')
          evalInter (Con cid (map Var xs), cs'')
    Fin a -> return $ Fin a


addCont :: Conts -> Either [Conts] ([Conts],SemiAtom, [Conts]) ->
                     Either [Conts] ([Conts],SemiAtom, [Conts]) 
addCont c (Left cs) = Left (c : cs)
addCont c (Right (cs, e, cs')) = Right (c : cs, e, cs')

type SemiAtom = (CId, [Alt [Atom]])

transposeSemiAtom :: [Alt [Atom]] -> [[Alt Expr]]
transposeSemiAtom = transpose . map (fmap (fmap (flip Expr [])) . sequence)
       
inlineFunc :: Monad m => Func -> ReachT m Expr
inlineFunc (Func e vs) = do 
  i <- use nextEVar
  nextEVar += vs
  return (e i) 

--lalt :: Monad m => Alt Expr -> ReachT m (Alt Expr)
--lalt (Alt cid vs e) = uncurry (Alt cid) <$> lbinds vs e
--lalt (AltDef e) = return $ AltDef e

interweaver :: Monad m => [Conts] -> ReachT m (Either [Conts] ([Conts], SemiAtom, [Conts]))
interweaver [] = return (Left [])
interweaver (Apply e : cs) = addCont (Apply e) <$> interweaver cs
interweaver (Branch as : cs) = do --addCont (Branch as) <$> interweaver cs
  i <- interweave as 
  case i of 
    Left as' -> addCont (Branch as') <$> interweaver cs
    Right e -> return $ Right ([], e, cs)

interweave :: Monad m => [Alt Expr] -> ReachT m (Either [Alt Expr] SemiAtom)
interweave as = do
  as' <- (mapM . mapM) (bindLets >=> evalInter) as
--  return (Left ((fmap . fmap) (either (uncurry Expr) (flip Expr [] . fst)) as'))
  case consol' as' of 
    Left as'' -> return (Left as'')
    Right as'' -> do --return . Left $ (fmap . fmap) (flip Expr []) as''
      case consolidate as'' of
        Nothing -> return . Left $ (fmap . fmap) (flip Expr []) as''
        Just e -> return . Right $ e
  
consol' :: [Alt Susp] -> Either [Alt Expr] [Alt Atom]
consol' [] = Right []
consol' (AltDef e : _) = case e of
  Fin e -> Right [AltDef e]
  e -> Left [AltDef $ suspToExpr e]
consol' (Alt cid vs e : as) = case consol' as of
  Left as -> Left (Alt cid vs (suspToExpr e) : as)
  Right as -> case e of
    Fin e -> Right (Alt cid vs e : as)
    e -> Left (Alt cid vs (suspToExpr e) : (fmap . fmap) (flip Expr []) as)

suspToExpr :: Susp -> Expr
suspToExpr (Fin a) = Expr a []
suspToExpr (SuspL v cs) = Expr (Var v) cs
suspToExpr (Susp x cs) = Expr (FVar x) cs


consolidate :: [Alt Atom] -> Maybe SemiAtom
consolidate [Alt c vs (Con cid es)] = Just (cid, [Alt c vs es]) 
consolidate [AltDef (Con cid es)] = Just (cid, [AltDef es])
consolidate (Alt c vs (Con cid es) : as) = do
   (cid', ars) <- consolidate as
   if cid' == cid
     then return (cid', Alt c vs es : ars)
     else Nothing

reduceTrace :: Monad m => Reduce m -> Reduce m
reduceTrace r e cs = do
  s <- get
  trace (printDoc (printState (Expr e cs) s)) $ reduce r e cs

traceSusp :: Monad m => Susp -> ReachT m Susp
traceSusp e = do
  traceExpr (suspToExpr e) >> return e 

traceExpr :: Monad m => Expr -> ReachT m Expr
traceExpr e = do
  s <- get
  trace (printDoc (printState e s)) $ return e 

bindLets :: Monad m => Expr -> ReachT m Expr'
bindLets (Let x e e') = do
  e'' <- bind x e
  bindLets e'
bindLets (Expr a cs) = return (a, cs)

                       
--reduceTrace :: Monad m => Reduce m -> Reduce m
--reduceTrace r e cs = do
--  traceExpr (Expr e cs)
--  reduce r e cs
  
reduce :: Monad m => Reduce m -> Reduce m
reduce r (Lam x e)    [] = return . Fin $ Lam x e
reduce r (Con cid es) [] = return . Fin $ Con cid es
--reduce r (Lam x e') (Apply (Expr a []) : cs) = do
--  e'' <- replaceLVar x a e'
--  (e , cs') <- bindLets e''
--  r e (cs' ++ cs) 
reduce r (Lam x e') (Apply e : cs) = do
  bind x e
  (e'', cs') <- bindLets e'
  r e'' (cs' ++ cs)

--  vs <- scopingExpr e'
--  vs' <- scopeExpr e
--  if null (I.intersection vs vs') 
--    then do
--    else do
--       traceExpr (Expr (Lam x e') (Apply e : cs) )
--       error "scoping"
reduce r (Con cid es) (Branch as : cs) = do
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
--reduce r (Var x) cs = return $ SuspL x cs

match :: Monad m => CId -> [Atom] -> [Alt Expr] -> ReachT m Expr
match  cid es (Alt cid' xs c : as)
  | cid == cid' = binds xs (map atom es) >> return c
--      vs <- scopingExpr c
--      vs' <- I.unions <$> mapM scopeAtom es
--      if null (I.intersection (I.union (I.fromList (zip xs (repeat ()))) vs) vs')
--         then   
--         else error "scoping"
  | otherwise   = match cid es as
match  cid es (AltDef e : _) = return e
match _ _ [] = error "REACH_ERROR: no match for constructor in case"
                         

binds :: Monad m => [LId] -> [Expr] -> ReachT m ()
binds vs es = mapM_ (uncurry bind) (zip vs es)
--binds _ _ _ = error "Constructor / Alterenative variable mismatch"
--
---- Bind x to e in e', A new environment variable, ex, is created for x and the
---- variable x is replaced with ex in e'. Then ex is bound to e in the environment.
bind :: Monad m => LId -> Expr -> ReachT m ()
bind v e = env . at v ?= e
--  ev <- evar e
--  replaceLVar v (EVar ev) c 

--lbind :: Monad m => LId -> Expr -> ReachT m (LId, Expr)
--lbind v e = do
--  lv <- lvar
--  e' <- replaceLVar v (LVar lv) e
--  return (lv, e')
--
--
--lbinds :: Monad m => [LId] -> Expr -> ReachT m ([LId], Expr)
--lbinds [] e = return ([], e)
--lbinds (v : vs) e = do
--  (v', e') <- lbind v e
--  (vs',e'') <- lbinds vs e'
--  return (v' : vs', e'')


lvar :: Monad m => ReachT m LId  
lvar = do
  lv <- use nextLVar
  nextLVar -= 1
  return lv
--ubind :: Monad m => LId -> Expr -> ReachT m Expr 
--ubind x c = do
--  ex <- uevar 
--  replaceLVar x (EVar ex) c
--
--ubinds :: Monad m => [LId] -> Expr -> ReachT m Expr 
--ubinds vs e = foldM (\e' v -> ubind v e') e vs

evar :: Monad m => Expr -> ReachT m EId
evar e = do
  ex <- use nextEVar
  nextEVar += 1
  env . at ex ?= e
  return ex

evars :: Monad m => [Expr] -> ReachT m [EId]
evars = mapM evar 

--replaceLVars :: Monad m => [LId] -> [Atom] -> Expr -> ReachT m Expr
--replaceLVars [] [] e = return e
--replaceLVars (v : vs) (e : es) e' = replaceLVar v e e' >>= replaceLVars vs es

replaceLVar :: Int -> Expr -> Expr
replaceLVar v (Let x e e') = Let (x + v) (replaceLVar v e) (replaceLVar v e')
replaceLVar v (Expr e cs) = Expr (replaceAtom v e) (map replaceConts cs)
   where
     replaceConts (Apply e) = Apply (replaceLVar v e)
     replaceConts (Branch as) = Branch (map replaceAlt as)

     replaceAlt (Alt c vs e) = Alt c (map (v+) vs) (replaceLVar v e)
     replaceAlt (AltDef e) = AltDef (replaceLVar v e)

replaceAtom :: Int -> Atom -> Atom
replaceAtom v (Fun f) = Fun f
replaceAtom v (Var x) = Var (v + x)
--  e <- use (env . at' x)
--  e' <- replaceLVar v a e
--  env . at x ?= e'
--  return $ Var x
--replaceAtom v a (LVar v')
--  | v == v'   = return a 
--  | otherwise = return $ LVar v' 
replaceAtom v (Lam x e) = Lam (v + x) (replaceLVar v e)
replaceAtom v (FVar x) = FVar x
replaceAtom v (Con c as) = Con c (map (replaceAtom v) as)

newFVars :: Monad m => Int -> ReachT m [FId]
newFVars n = replicateM n newFVar

newFVar :: Monad m => ReachT m FId
newFVar = do
  x <- use nextFVar
  nextFVar += 1
  freeDepth . at x ?= 1
  topFrees %= (x :)
  return x


--scopeExpr :: Monad m => Expr -> ReachT m (IntMap ())
--scopeExpr (Let v e e') = I.union <$> scopeExpr e <*> (I.delete v <$> (scopeExpr e'))
--scopeExpr (Expr a cs) =  I.union <$> scopeAtom a
--                                 <*> foldM (\a b -> I.union a <$> (scopeConts b)) I.empty cs
--
--scopeAtom :: Monad m => Atom -> ReachT m (IntMap ())
--scopeAtom (Fun _) = return $ I.empty
--scopeAtom (Var v) = use  (env . at' v) >>= scopeExpr
--
--scopeAtom (LVar v) = return $ I.singleton v () 
--scopeAtom (FVar _) = return $ I.empty
--scopeAtom (Lam v e) = I.delete v <$> scopeExpr e
--scopeAtom (Con c as) = I.unions <$> mapM scopeAtom as 
--
--scopeConts :: Monad m => Conts -> ReachT m (IntMap ())
--scopeConts (Branch _ as) = I.unions <$> mapM scopeAlt as
--    where scopeAlt (Alt _ vs e) = I.difference
--                                  <$> scopeExpr e
--                                  <*> pure (I.fromList $ zip vs (repeat ())) 
--          scopeAlt (AltDef e) = scopeExpr e
--scopeConts (Apply e) = scopeExpr e 
--
--scopingExpr :: Monad m => Expr -> ReachT m (IntMap ())
--scopingExpr (Let v e e') = I.union
--                           <$> scopingExpr e
--                           <*> (I.insert v () <$> scopingExpr e')
--scopingExpr (Expr a cs) =  I.union <$> scopingAtom a
--                                 <*> foldM (\a b -> I.union a <$> (scopingConts b)) I.empty cs
--
--scopingAtom :: Monad m => Atom -> ReachT m (IntMap ())
--scopingAtom (Fun _) = return $ I.empty
--scopingAtom (EVar v) = use  (env . at' v) >>= scopingExpr
--
--scopingAtom (LVar _) = return $ I.empty
--scopingAtom (FVar _) = return $ I.empty
--scopingAtom (Lam v e) = I.insert v () <$> scopingExpr e
--scopingAtom (Con c as) = I.unions <$> mapM scopingAtom as 
--
--scopingConts :: Monad m => Conts -> ReachT m (IntMap ())
--scopingConts (Branch _ as) = I.unions <$> mapM scopingAlt as
--    where scopingAlt (Alt _ vs e) = I.union 
--                                  <$> scopingExpr e
--                                  <*> pure (I.fromList $ zip vs (repeat ())) 
--          scopingAlt (AltDef e) = scopingExpr e
--scopingConts (Apply e) = scopingExpr e 
