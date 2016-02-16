module Reach.Eval.Full where

import Reach.Eval.Lazy
import Reach.Eval.Reduce
import Reach.Eval.Expr
import Reach.Eval.Monad
import Reach.Eval.Gen
import Reach.Eval.Env
import Reach.Lens
import Reach.Printer
import Data.List
import Data.IntMap (IntMap)
import qualified Data.IntMap as I

--evalBase :: MonadChoice m => Expr' -> ReachT m Atom 
--evalBase e = do
--  r <- evalInter e
--  case r of
--    Fin (Lam v e) -> do
--      x <- newFVar
--      evalBase (Lam v e, [Apply . atom . FVar $ x])
--    Fin a -> return a
--    Susp x (Branch as : cs) -> do
--       (cid, xs) <- choose x
--       evalBase (Con cid (map FVar xs), Branch as : cs)
--    o -> error (show o)
--
--evalInter :: Monad m => Expr' -> ReachT m Susp
--evalInter (e, cs) = do
--  c <- fix reduce e cs
--  case c of
--    SuspL v cs' -> return $ SuspL v cs'
--    Susp x cs' -> do 
--      i <- interweaver cs' 
--      case i of
--        Left cs'' -> return $ Susp x cs''
--        Right (bcs, (cid, as), cs'') -> do
--          subj <- evar (Expr (FVar x) bcs)
--          let es = fmap (Expr (Var subj) . return . Branch) (transposeSemiAtom as)
--          xs <- evars es 
--          evalInter (Con cid (map Var xs), cs'')
--    Fin a -> return $ Fin a
--
--
--addCont :: Conts -> Either [Conts] ([Conts],SemiAtom, [Conts]) ->
--                     Either [Conts] ([Conts],SemiAtom, [Conts]) 
--addCont c (Left cs) = Left (c : cs)
--addCont c (Right (cs, e, cs')) = Right (c : cs, e, cs')
--
--type SemiAtom = (CId, [Alt [Atom]])
--
--transposeSemiAtom :: [Alt [Atom]] -> [[Alt Expr]]
--transposeSemiAtom = transpose . map (fmap (fmap (flip Expr [])) . sequence)
--
--interweaver :: Monad m => [Conts] -> ReachT m (Either [Conts] ([Conts], SemiAtom, [Conts]))
--interweaver [] = return (Left [])
--interweaver (Apply e : cs) = addCont (Apply e) <$> interweaver cs
--interweaver (Branch as : cs) = do --addCont (Branch as) <$> interweaver cs
--  i <- interweave as 
--  case i of 
--    Left as' -> addCont (Branch as') <$> interweaver cs
--    Right e -> return $ Right ([], e, cs)
--
--interweave :: Monad m => [Alt Expr] -> ReachT m (Either [Alt Expr] SemiAtom)
--interweave as = do
--  as' <- (mapM . mapM) (bindLets >=> evalInter) as
--  case consol' as' of 
--    Left as'' -> return (Left as'')
--    Right as'' -> do --return . Left $ (fmap . fmap) (flip Expr []) as''
--      case consolidate as'' of
--        Nothing -> return . Left $ (fmap . fmap) (flip Expr []) as''
--        Just e -> return . Right $ e
--  
--consol' :: [Alt Susp] -> Either [Alt Expr] [Alt Atom]
--consol' [] = Right []
--consol' (AltDef e : _) = case e of
--  Fin e -> Right [AltDef e]
--  e -> Left [AltDef $ suspToExpr e]
--consol' (Alt cid vs e : as) = case consol' as of
--  Left as -> Left (Alt cid vs (suspToExpr e) : as)
--  Right as -> case e of
--    Fin e -> Right (Alt cid vs e : as)
--    e -> Left (Alt cid vs (suspToExpr e) : (fmap . fmap) (flip Expr []) as)
--
--consolidate :: [Alt Atom] -> Maybe SemiAtom
--consolidate [Alt c vs (Con cid es)] = Just (cid, [Alt c vs es]) 
--consolidate [AltDef (Con cid es)] = Just (cid, [AltDef es])
--consolidate (Alt c vs (Con cid es) : as) = do
--   (cid', ars) <- consolidate as
--   if cid' == cid
--     then return (cid', Alt c vs es : ars)
--     else Nothing

