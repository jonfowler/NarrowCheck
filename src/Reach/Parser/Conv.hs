module Reach.Parser.Conv where

import qualified Data.IntMap as I
import Data.IntMap (IntMap)
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe

import Control.Lens
import Control.Monad.State
import Control.Monad.Except
import qualified Data.DList as D

import Control.Applicative

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
  _convertFId :: Conv S.VarId,
  _convertCon :: Conv S.ConId,
  _convertLocals :: Conv S.VarId
                       }

makeLenses ''Convert

--newtype MaxInt = MaxInt Int deriving (Num)
--
--instance Monoid MaxInt where
--  mempty = 0
--  mappend (MaxInt m) (MaxInt n) = MaxInt (max m n)

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
  { _convertFId = execState (setupConv (M.keys $ m ^. S.moduleDef)) emptyConv,
    _convertCon = execState (setupConv (M.keys $ m ^. S.moduleCon)) emptyConv,
    _convertLocals = emptyConv
  }

setupConv :: Ord a => [a] -> State (Conv a) ()
setupConv as = foldM_ (\_ v -> overConv id v) 0 as
                     
convModule :: S.Module -> Env
convModule m = Env {
             _funcs = I.fromList $ map (convFun c) (M.elems $ m ^. S.moduleDef),
             _frees = I.empty,
             _env = I.empty,
             _nextVar = 0,

             _funcNames = c ^. convertFId . mapFromInt,
             _funcIds = c ^. convertFId . mapToInt,

             _constrNames = c ^. convertCon . mapFromInt 
             }
  where c = setupConvert m

convFun :: Convert -> S.Def ->  (FId, Func)
convFun c def = case runExcept . runStateT s $ c of
  Left e -> error e
  Right (e , c') -> (
    fromMaybe (error "Function not found") (c' ^. convertFId . mapToInt . at (def ^. S.defName)) ,
    Func {_body = e, _vars = (c' ^. convertLocals . nextInt)})
 where s = convArgs (def ^. S.defArgs) <*> convExpr (def ^. S.defBody)

convArgs :: [S.VarId] -> ConvertM (Expr -> Expr)
convArgs [] = return id
convArgs (v : vs) = do
  i <- overConv convertLocals v 
  f <- convArgs vs
  return (Lam i . f)

convExpr :: S.Exp -> ConvertM Expr
convExpr (S.Var vid) =  LVar <$> viewConv convertLocals vid
                    <|> Fun <$> viewConv convertFId vid
                    <|> throwError ("Variable " ++ show vid ++ " is not in local or function names")
convExpr (S.ConE cid) = flip Con empty <$> viewConv convertCon cid
                     <|> throwError ("Constructor " ++ show cid ++ " does not exist")
convExpr (S.App e e') = App <$> (convExpr e) <*> (convExpr e') 
convExpr (S.Parens e) = convExpr e
convExpr (S.Case e as) = do
  e' <- convExpr e
  as' <- mapM convAlt as
  return (Case e' as')


convAlt :: S.Alt -> ConvertM Alt
convAlt (S.Alt (S.Con cid xs)  e) = do
  cs <- viewCons cid
  vs <- mapM (overConv convertLocals) xs
  e' <- convExpr e
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
