module Reach.Parser.ConReduce where

import Reach.Parser.Module

{- The following module evaluates the applications of a constructor
and then atomises the fields of the constructor-}


partial :: Expr -> Expr
partial (Case e as) = Case (partial e) (map partialAlt as)
    where partialAlt (Alt c e) = Alt c (partial e)
partial (App e e') = case partial e of
  ConE cid es -> ConE cid (es ++ [partial e'])
  ne -> App ne (partial e')
partial (Parens e) = Parens (partial e)
partial (ConE cid es) = ConE cid (map partial es)
partial (Var v) = Var v

--partial :: Expr -> Expr
--partial e = case subPartial e of
--  ConE cid es -> let (f , e) = atomiser (ConE cid es) 
--                 in f e
--  ne -> ne

--atomiseCon :: CId -> [Expr] -> Expr
--atomiseCon cid es = 

--atomiser :: Expr -> ((Expr -> Expr) , Expr)
--atomiser (ConE cid es) = fmap (ConE cid) $ atomising es
--atomiser (Var x) = (id , Var x)
--
--atomiser (Parens e) = fmap Parens $ atomiser e 
--atomiser e = 
--
--atomising :: [Expr] -> ((Expr -> Expr) , [Expr])
--atomising = foldr (\(f , e) (g , es) -> (f . g, e : es)) (id , [])
--          . map atomiser
--convExpr :: S.Exp -> ConvertM Cont
--convExpr e = do
--  e' <- convSubExpr e 
--  case e' of
--    (Con cid es) -> atomiseCon cid es 
--    _ -> return e'
--
--atomiseCon :: CId -> [Expr] -> ConvertM Expr
--atomiseCon cid es = do
--  n <- argNum cid
--  (f , es') <- addLams (n - length es)
--  (g , e'') <- atomiser (Con cid (es ++ es'))
--  return (f . g $ e'')
--
--addLams :: Int -> ConvertM (Expr -> Expr, [Expr])
--addLams 0 = return (id , [])
--addLams n = do
--  v <- overConv convertLocals  ""
--  (f , es) <- addLams (n - 1)
--  return (Lam v . f , LVar v : es )
--
--
--argNum :: CId -> ConvertM Int
--argNum cid = do
--  c <- use (convertCon . mapFromInt . at' cid)
--  use (conInfo . at' c)
--
--atomiser :: Expr -> ConvertM (Expr -> Expr, Expr)
--atomiser (Con cid es) = do
--  (f , es') <- atomising es
--  return (f , Con cid es')
--atomiser (Lam v e) = return (id , Lam v e)
--atomiser (LVar x) = return (id , LVar x)
--atomiser (Fun fid) = return (id , Fun fid)
--atomiser e = do
--  x <- overConv convertLocals  ""
--  return (Let x e , LVar x)
--
--atomising :: [Expr] -> ConvertM (Expr -> Expr, [Expr])
--atomising [] = return (id, [])
--atomising (e : es) = do
--  (f,e') <- atomiser e
--  (g,es') <- atomising es
--  return (f . g, e : es')
--
--convAlt :: S.Alt -> ConvertM (Alt Expr)
--convAlt (S.Alt (S.Con cid xs)  e) = do
--  cs <- viewCons cid
--  vs <- mapM (overConv convertLocals) xs
--  e' <- convExpr e
--  return (Alt cs vs e') 

