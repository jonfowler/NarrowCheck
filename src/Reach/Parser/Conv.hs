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

import Reach.Parser.Desugar
import Reach.Parser.Module
import Reach.Parser.PExpr
import Reach.Eval.Expr
import Reach.Eval.Env

data Conv a = Conv
  { _mapToInt :: Map a Int
  , _mapFromInt ::  IntMap a
  , _nextInt :: Int
  }

makeLenses ''Conv

data Convert = Convert {
  _convertFuncId :: Conv VarId,
  _convertCon :: Conv ConId,
  _convertLocals :: Conv VarId,
  _conInfo :: Map ConId Int 
                       }

makeLenses ''Convert

type ConvertM = StateT Convert (Except String)  --ConvertM {runConvert :: (Convert -> (a , Int))}

viewConv :: Ord a => Getter Convert (Conv a) -> a -> ConvertM Int
viewConv f vid = do
  r <- use (f . mapToInt . at vid)
  case r of
    Just v -> return v
    Nothing -> throwError ""

viewCons :: ConId -> ConvertM Int
viewCons cid = viewConv convertCon cid
                     <|> throwError ("Constructor " ++ show cid ++ " does not exist")

setupConvert :: Module -> Convert 
setupConvert m = Convert
  { _convertFuncId = execState (setupConv (M.keys $ m ^. moduleDef)) emptyConv,
    _convertCon = execState (setupConv (M.keys $ m ^. moduleCon)) emptyConv,
    _convertLocals = emptyConv,
    _conInfo = fmap length $ m ^. moduleCon 
  }

setupConv :: Ord a => [a] -> State (Conv a) ()
setupConv as = foldM_ (\_ v -> overConv id v) 0 as
                     
convModule :: Int -> Module -> Env
convModule i m = Env {
             _funcs = I.fromList $ map (convFun c) (M.toList $ m ^. moduleDef),

             _free = I.empty,
             _nextFVar = 0,
             _freeDepth = I.empty,
             _maxDepth = i,
             _topFrees = [],

             _env = I.empty,
             _nextEVar = 0,
             _nextLVar = -1,

             _funcNames = c ^. convertFuncId . mapFromInt,
             _funcIds = c ^. convertFuncId . mapToInt,

             _constrNames = c ^. convertCon . mapFromInt,
             _constrIds = c ^. convertCon . mapToInt 
             }
  where c = setupConvert m

convFun :: Convert -> (VarId, [PDef]) -> (FuncId, Func)
convFun c (vid, qs) = case runExcept . runStateT s $ c of
  Left e -> error e
  Right (e , c') -> (
    fromMaybe (error "Function not found") (c' ^. convertFuncId . mapToInt . at vid) ,
    Func {_body = e,
          _vars = c' ^. convertLocals . nextInt
          })
 where s = convExpr (desugar qs) []
         --convArgs (map aVar $ def ^. defArgs) <*> convExpr (desugar $ def ^. defBody) []

aVar :: Pattern -> VarId
aVar (PatVar x) = x

convArgs :: [VarId] -> ConvertM (Expr -> Expr)
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
 
convExpr :: PExpr -> [Conts] -> ConvertM Expr
convExpr (PVar vid) cs = Expr <$> (LVar <$> viewConv convertLocals vid) <*> pure cs
                    <|> Expr <$> (Fun <$> viewConv convertFuncId vid) <*> pure cs
                    <|> throwError ("Variable " ++ show vid ++ " is not in local or function names")
convExpr (PCon cid es) cs = do
  es' <- mapM (flip convExpr []) es
  (f, as) <- atomises es'
  c <- viewConv convertCon cid
  return . f $ Expr (Con c as) cs
convExpr (PApp f e) cs = do
  e' <- convExpr e []
  convExpr f (Apply e' : cs)
convExpr (PLam v e) cs = do
  v' <- overConv convertLocals v
  e' <- convExpr e []
  return (Expr (Lam v' e') [])
  
convExpr (PParens e) cs = convExpr e cs
convExpr (PCase e as) cs = do
  as' <- mapM convAlt as
  convExpr e (Branch False as' : cs)


convAlt :: PAlt PExpr -> ConvertM (Alt Expr)
convAlt (PAlt (PatCon cid xs)  e) = do
  cs <- viewCons cid
  vs <- mapM (overConv convertLocals) (map aVar xs)
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
