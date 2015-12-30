module Reach.Parser.Conv where

import qualified Data.IntMap as I
import Data.IntMap (IntMap)
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe

import Reach.Lens
import Control.Monad.State
import Control.Monad.Except

import Control.Applicative

import Reach.Parser.ConReduce
import qualified Reach.Parser.Module as S
import Reach.Eval.Expr
import Reach.Eval.Env

data Conv a = Conv
  { _mapToInt :: Map a Int
  , _mapFromInt ::  IntMap a
  , _nextInt :: Int
  }

makeLenses ''Conv

data Convert = Convert {
  _convertFuncId :: Conv S.VarId,
  _convertCon :: Conv S.ConId,
  _convertLocals :: Conv S.VarId,
  _conInfo :: Map S.ConId Int 
                       }

makeLenses ''Convert

type ConvertM = StateT Convert (Except String)  --ConvertM {runConvert :: (Convert -> (a , Int))}

viewConv :: Ord a => Getter Convert (Conv a) -> a -> ConvertM Int
viewConv f vid = do
  r <- use (f . mapToInt . at vid)
  case r of
    Just v -> return v
    Nothing -> throwError ""

viewCons :: S.ConId -> ConvertM Int
viewCons cid = viewConv convertCon cid
                     <|> throwError ("Constructor " ++ show cid ++ " does not exist")

setupConvert :: S.Module -> Convert 
setupConvert m = Convert
  { _convertFuncId = execState (setupConv (M.keys $ m ^. S.moduleDef)) emptyConv,
    _convertCon = execState (setupConv (M.keys $ m ^. S.moduleCon)) emptyConv,
    _convertLocals = emptyConv,
    _conInfo = fmap (length . S._conArgs) $ m ^. S.moduleCon 
  }

setupConv :: Ord a => [a] -> State (Conv a) ()
setupConv as = foldM_ (\_ v -> overConv id v) 0 as
                     
convModule :: S.Module -> Env
convModule m = Env {
             _funcs = I.fromList $ map (convFun c) (M.elems $ m ^. S.moduleDef),

             _free = I.empty,
             _nextFVar = 0,

             _env = I.empty,
             _nextEVar = 0,

             _funcNames = c ^. convertFuncId . mapFromInt,
             _funcIds = c ^. convertFuncId . mapToInt,

             _constrNames = c ^. convertCon . mapFromInt 
             }
  where c = setupConvert m

convFun :: Convert -> S.Def ->  (FuncId, Func)
convFun c def = case runExcept . runStateT s $ c of
  Left e -> error e
  Right (e , c') -> (
    fromMaybe (error "Function not found") (c' ^. convertFuncId . mapToInt . at (def ^. S.defName)) ,
    Func {_body = e, _vars = (c' ^. convertLocals . nextInt)})
 where s = convArgs (def ^. S.defArgs) <*> convExpr (partial $ def ^. S.defBody) []

convArgs :: [S.VarId] -> ConvertM (Expr -> Expr)
convArgs [] = return id
convArgs (v : vs) = do
  i <- overConv convertLocals v 
  f <- convArgs vs
  return (flip Expr [] . Lam i . f)

atomise :: Expr -> ConvertM (Expr -> Expr, Atom)
atomise (Let x e e') = do
  (f , a') <- atomise e'
  return (Let x e . f , a')
atomise (Expr e []) = return (id, e)
atomise e = do
  x <- overConv convertLocals  ""
  return (Let x e , LVar x)

atomises :: [Expr] -> ConvertM (Expr -> Expr, [Atom])
atomises es = foldr (\(f , a) (g , as) -> (f . g, a : as)) (id , []) <$> mapM atomise es
 
convExpr :: S.Expr -> [Conts] -> ConvertM Expr
convExpr (S.Var vid) cs = Expr <$> (LVar <$> viewConv convertLocals vid) <*> pure cs
                    <|> Expr <$> (Fun <$> viewConv convertFuncId vid) <*> pure cs
                    <|> throwError ("Variable " ++ show vid ++ " is not in local or function names")
convExpr (S.ConE cid es) cs = do
  es' <- mapM (flip convExpr []) es
  (f, as) <- atomises es'
  c <- viewConv convertCon cid
  return . f $ Expr (Con c as) cs
convExpr (S.App f e) cs = do
  e' <- convExpr e []
  convExpr f (Apply e' : cs)
convExpr (S.Parens e) cs = convExpr e cs
convExpr (S.Case e as) cs = do
  as' <- mapM convAlt as
  convExpr e (Branch as' : cs)

--convExpr :: S.Expr -> ConvertM Expr
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
--  
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

convAlt :: S.Alt -> ConvertM (Alt Expr)
convAlt (S.Alt (S.Con cid xs)  e) = do
  cs <- viewCons cid
  vs <- mapM (overConv convertLocals) xs
  e' <- convExpr e []
  return (Alt cs vs e') 

overConv :: (MonadState s m, Ord a) => Simple Lens s (Conv a) -> a -> m Int
overConv l a = do
     i <- use (l . nextInt)
     l . mapToInt . at a ?= i
     l . mapFromInt . at i ?= a
     l . nextInt .= i + 1
     return i

updConv :: (MonadState s m, Ord a) => Simple Lens s (Conv a) -> a -> m Int
updConv l a = do
     r <- use (l . mapToInt . at a)
     case r of
       Nothing -> overConv l a 
       Just i -> return i

emptyConv :: Conv a
emptyConv = Conv
  { _mapToInt = M.empty,
    _mapFromInt = I.empty,
    _nextInt = 0
  }
