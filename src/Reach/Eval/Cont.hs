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
      fexpr <- use (funcs . at' fid . body)
      ts <- use (funcArgTypes . at' fid)
      xs <- mapM (fvar 0) ts
      topFrees .= xs
      (e, cs) <- bindLets fexpr
      return (foldr (\x (e', cs') -> (e, Apply (atom $ FVar x) : cs')) (e, cs) xs)


evalLazy :: MonadChoice m => Expr' -> ReachT m Atom
evalLazy (e, conts) = do
   c <- fix reduceTrace e conts
   case c of 
     Fin (Lam v e) -> do
        x <- newFVar
        evalLazy (Lam v e, [Apply . atom . FVar $ x])
     Fin a -> return a
     Susp x (Branch _ as : cs) -> do
--       (cid,vs) <- choose x (map (\(Alt c vs e) -> (c,length vs)) as)
--       xs <- fvars x vs
--       free . at x ?= (cid, xs)
       (cid, xs) <- choose x
       evalLazy (Con cid (map FVar xs), Branch True as : cs)
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
    Susp x (Branch _ as : cs) -> do
--       traceExpr' (FVar x, Branch as : cs)
--       (cid,vs) <- choose x (map (\(Alt c vs e) -> (c,length vs)) as)
--       xs <- fvars x vs
--       free . at x ?= (cid, xs)
       (cid, xs) <- choose x
       evalBase (Con cid (map FVar xs), Branch True as : cs)
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
          let es = fmap (Expr (EVar subj) . return . Branch True) (transposeSemiAtom as)
          xs <- evars es 
--          traceExpr (Expr (Con cid (map EVar xs)) cs'')
          evalInter (Con cid (map EVar xs), cs'')
    Fin a -> return $ Fin a


addCont :: Conts -> Either [Conts] ([Conts],SemiAtom, [Conts]) ->
                     Either [Conts] ([Conts],SemiAtom, [Conts]) 
addCont c (Left cs) = Left (c : cs)
addCont c (Right (cs, e, cs')) = Right (c : cs, e, cs')

type SemiAtom = (CId, [Alt [Atom]])

transposeSemiAtom :: [Alt [Atom]] -> [[Alt Expr]]
transposeSemiAtom = transpose . map (fmap (fmap (flip Expr [])) . sequence)
       

lalt :: Monad m => Alt Expr -> ReachT m (Alt Expr)
lalt (Alt cid vs e) = uncurry (Alt cid) <$> lbinds vs e
lalt (AltDef e) = return $ AltDef e

interweaver :: Monad m => [Conts] -> ReachT m (Either [Conts] ([Conts], SemiAtom, [Conts]))
interweaver [] = return (Left [])
interweaver (Apply e : cs) = addCont (Apply e) <$> interweaver cs
interweaver (Branch t as : cs) = do --addCont (Branch as) <$> interweaver cs
  as' <- if t then return as else mapM lalt as
  i <- interweave as' 
  case i of 
    Left as'' -> addCont (Branch True as'') <$> interweaver cs
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
suspToExpr (SuspL v cs) = Expr (LVar v) cs
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
  (e'', cs') <- bind x e e' >>= bindLets 
  r e'' (cs' ++ cs)

--  vs <- scopingExpr e'
--  vs' <- scopeExpr e
--  if null (I.intersection vs vs') 
--    then do
--    else do
--       traceExpr (Expr (Lam x e') (Apply e : cs) )
--       error "scoping"
reduce r (Con cid es) (Branch _ as : cs) = do
  (e, cs') <- match cid es as >>= bindLets
  r e (cs' ++ cs)
reduce r (Con cid es) (Apply e : cs) = fail "Partial constructors not implemented yet"
reduce r (Fun fid) cs = do
  Expr e cs' <- use (funcs . at' fid . body) 
  r e (cs' ++ cs) 
reduce r (EVar ev) cs = do
  (e, cs') <- use (env . at' ev) >>= bindLets
  s <- r e cs'
  case s of
    Susp x cs'' -> do
      env . at ev ?= Expr (FVar x) cs''
      return (Susp x (cs'' ++ cs))
    SuspL v cs'' -> do
      env . at ev ?= Expr (LVar v) cs''
      return (SuspL v (cs'' ++ cs))
    Fin a -> do
      env . at ev ?= Expr a [] 
      r a cs
reduce r (FVar x) cs = do
  c <- use (free . at x)
  case c of
    Just (cid, fids) -> r (Con cid (map FVar fids)) cs
    Nothing -> return $ Susp x cs
reduce r (LVar x) cs = return $ SuspL x cs

match :: Monad m => CId -> [Atom] -> [Alt Expr] -> ReachT m Expr
match  cid es (Alt cid' xs c : as)
  | cid == cid' = return $ LMap (I.fromList (zip xs es)) c
--      vs <- scopingExpr c
--      vs' <- I.unions <$> mapM scopeAtom es
--      if null (I.intersection (I.union (I.fromList (zip xs (repeat ()))) vs) vs')
--         then   
--         else error "scoping"
  | otherwise   = match cid es as
match  cid es (AltDef e : _) = return e
match _ _ [] = error "REACH_ERROR: no match for constructor in case"
                         

binds :: Monad m => [LId] -> [Expr] -> Expr -> ReachT m Expr 
binds (v : vs) (e : es) e' = bind v e e' >>= binds vs es
binds [] [] e' = return e'
binds _ _ _ = error "Constructor / Alterenative variable mismatch"

-- Bind x to e in e', A new environment variable, ex, is created for x and the
-- variable x is replaced with ex in e'. Then ex is bound to e in the environment.
bind :: Monad m => LId -> Expr -> Expr -> ReachT m Expr 
bind v e c = do
  ev <- evar e
  return $ LMap (I.singleton v (EVar ev)) c

lbind :: Monad m => LId -> Expr -> ReachT m (LId, Expr)
lbind v e = do
  lv <- lvar
--  e' <- replaceLVar v (LVar lv) e
  return (lv, LMap (I.singleton v (LVar lv)) e)


lbinds :: Monad m => [LId] -> Expr -> ReachT m ([LId], Expr)
lbinds [] e = return ([], e)
lbinds (v : vs) e = do
  (v', e') <- lbind v e
  (vs',e'') <- lbinds vs e'
  return (v' : vs', e'')


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

bindLets :: Monad m => Expr -> ReachT m Expr'
bindLets (Let x e e') = do
  e'' <- bind x e e'  
  bindLets e''
bindLets (LMap f e) = bindLocals f e
bindLets (Expr a cs) = return (a, cs)

bindLocals :: Monad m => IntMap Atom -> Expr -> ReachT m Expr'
bindLocals f e | I.null f = bindLets e
               | otherwise = bindLocals' f e

bindLocals' :: Monad m => IntMap Atom -> Expr -> ReachT m Expr'
bindLocals' f (LMap g e) = do
--  fg <- sequence $ I.unionWith (\a _ -> a >>= substAtom g) (fmap return f) (fmap return g)
  let fg = I.unionWith (\a _ -> error "bindLocals.. overwrite") f g
  bindLocals' fg e
bindLocals' f (Let x e e') = do
  ev <- evar (LMap f e)
  let fg = I.insertWith (error "bindLocals.. overwrite") x (LVar ev) f
  bindLocals' fg e'
bindLocals' f (Expr a cs) = do
  a' <- substAtom f a
  cs' <- mapM (substCont f) cs
  return (a', cs')


--  bindLet x (LMap f e)

substCont :: Monad m => (IntMap Atom) -> Conts -> ReachT m Conts
substCont f (Apply e) = return $ Apply (LMap f e)
substCont f (Branch b as) = return $ Branch b (map substAlt as)
   where substAlt (Alt cid vs e) = Alt cid vs (LMap (foldr I.delete f vs) e)
         substAlt (AltDef e) = AltDef (LMap f e)

substAtom :: Monad m => (IntMap Atom) -> Atom -> ReachT m Atom
substAtom f (Fun g) = return $ Fun g
substAtom f (EVar v) = do
  env . at' v %= LMap f 
  return (EVar v)
substAtom f (LVar v) = return $ case I.lookup v f of
  Nothing -> LVar v
  Just a -> a
substAtom f (Lam v e) = return $ Lam v (LMap (I.delete v f) e)
substAtom f (FVar x) = return $ FVar x
substAtom f (Con cid es) = Con cid <$> mapM (substAtom f) es
 

--replaceLVars :: Monad m => [LId] -> [Atom] -> Expr -> ReachT m Expr
--replaceLVars [] [] e = return e
--replaceLVars (v : vs) (e : es) e' = replaceLVar v e e' >>= replaceLVars vs es
--
--replaceLVar :: Monad m => LId -> Atom -> Expr -> ReachT m Expr
--replaceLVar v a (Let x e e')
--  | x == v    = return $ Let x e e'
--  | otherwise = Let x <$> replaceLVar v a e <*> replaceLVar v a e'
--replaceLVar v a (Expr e cs) = Expr <$> replaceAtom v a e <*> mapM replaceConts cs
--   where
--     replaceConts (Apply e) = Apply <$> replaceLVar v a e
--     replaceConts (Branch t as) = Branch t <$> mapM replaceAlt as
--
--     replaceAlt (Alt c vs e)
--       | v `elem` vs  = return $ Alt c vs e
--       | otherwise    = Alt c vs <$> replaceLVar v a e
--     replaceAlt (AltDef e) = AltDef <$> replaceLVar v a e
--
--replaceAtom :: Monad m => LId -> Atom -> Atom -> ReachT m Atom
--replaceAtom v a (Fun f) = return $ Fun f
--replaceAtom v a (EVar x) = do
--  e <- use (env . at' x)
--  e' <- replaceLVar v a e
--  env . at x ?= e'
--  return $ EVar x
--replaceAtom v a (LVar v')
--  | v == v'   = return a 
--  | otherwise = return $ LVar v' 
--replaceAtom v a (Lam x e)
--  | v == x    = return $ Lam x e
--  | otherwise = Lam x <$> replaceLVar v a e
--replaceAtom v a (FVar x) = return $ FVar x
--replaceAtom v a (Con c as) = Con c <$> mapM (replaceAtom v a) as

newFVars :: Monad m => Int -> ReachT m [FId]
newFVars n = replicateM n newFVar

newFVar :: Monad m => ReachT m FId
newFVar = do
  x <- use nextFVar
  nextFVar += 1
  freeDepth . at x ?= 1
  topFrees %= (x :)
  return x


scopeExpr :: Monad m => Expr -> ReachT m (IntMap ())
scopeExpr (Let v e e') = I.union <$> scopeExpr e <*> (I.delete v <$> (scopeExpr e'))
scopeExpr (Expr a cs) =  I.union <$> scopeAtom a
                                 <*> foldM (\a b -> I.union a <$> (scopeConts b)) I.empty cs

scopeAtom :: Monad m => Atom -> ReachT m (IntMap ())
scopeAtom (Fun _) = return $ I.empty
scopeAtom (EVar v) = use  (env . at' v) >>= scopeExpr

scopeAtom (LVar v) = return $ I.singleton v () 
scopeAtom (FVar _) = return $ I.empty
scopeAtom (Lam v e) = I.delete v <$> scopeExpr e
scopeAtom (Con c as) = I.unions <$> mapM scopeAtom as 

scopeConts :: Monad m => Conts -> ReachT m (IntMap ())
scopeConts (Branch _ as) = I.unions <$> mapM scopeAlt as
    where scopeAlt (Alt _ vs e) = I.difference
                                  <$> scopeExpr e
                                  <*> pure (I.fromList $ zip vs (repeat ())) 
          scopeAlt (AltDef e) = scopeExpr e
scopeConts (Apply e) = scopeExpr e 

scopingExpr :: Monad m => Expr -> ReachT m (IntMap ())
scopingExpr (Let v e e') = I.union
                           <$> scopingExpr e
                           <*> (I.insert v () <$> scopingExpr e')
scopingExpr (Expr a cs) =  I.union <$> scopingAtom a
                                 <*> foldM (\a b -> I.union a <$> (scopingConts b)) I.empty cs

scopingAtom :: Monad m => Atom -> ReachT m (IntMap ())
scopingAtom (Fun _) = return $ I.empty
scopingAtom (EVar v) = use  (env . at' v) >>= scopingExpr

scopingAtom (LVar _) = return $ I.empty
scopingAtom (FVar _) = return $ I.empty
scopingAtom (Lam v e) = I.insert v () <$> scopingExpr e
scopingAtom (Con c as) = I.unions <$> mapM scopingAtom as 

scopingConts :: Monad m => Conts -> ReachT m (IntMap ())
scopingConts (Branch _ as) = I.unions <$> mapM scopingAlt as
    where scopingAlt (Alt _ vs e) = I.union 
                                  <$> scopingExpr e
                                  <*> pure (I.fromList $ zip vs (repeat ())) 
          scopingAlt (AltDef e) = scopingExpr e
scopingConts (Apply e) = scopingExpr e 
