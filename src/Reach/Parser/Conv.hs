{-# LANGUAGE TemplateHaskell #-}

module Reach.Parser.Conv where

import qualified Data.IntMap as I
import Data.IntMap (IntMap)
import qualified Data.Map as M
import Data.Map (Map)

import Control.Lens
import Control.Monad.State
import Control.Monad.Except
import qualified Data.DList as D

import Control.Applicative

import qualified Reach.Parser.Module as S
import Reach.Eval.Expr


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

convExpr :: S.Exp -> ConvertM Expr
convExpr (S.Var vid) =  Var <$> viewConv convertLocals vid
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

overConv :: Ord a => Simple Lens Convert (Conv a) -> a -> ConvertM Int
overConv l a = do
     i <- use (l . nextInt)
     l . mapToInt . at a ?= i
     l . mapFromInt . at i ?= a
     l . nextInt .= i + 1
     return i

updConv :: Ord a => Simple Lens Convert (Conv a) -> a -> ConvertM Int
updConv l a = do
     r <- use (l . mapToInt . at a)
     case r of
       Nothing -> overConv l a 
       Just i -> return i


--localVars :: (Conv S.VarId -> Conv S.VarId) -> ConvertM a -> ConvertM a
--localVars f (Convert m) = Convert $ m . (convertLocals %~ f)


--instance Functor ConvertM where
--  fmap f (ConvertM r) = ConvertM (r & mapped . _1 %~ f)
--
--instance Applicative ConvertM where
--  pure a = ConvertM $ \c -> (a , c ^. convertLocals . nextInt)
--  ConvertM r <*> ConvertM r' = ConvertM (\c -> let (f , m) = r c
--                                                   (a , n) = r' c
--                                               in (f a, max m n))
--
--instance Monad ConvertM where
--  return = pure
--  ConvertM m >>= f = ConvertM $ \c -> let (a , p) = m c 
--                                      in runConvert (f a) c & _2 %~ max p


emptyConv :: Conv a
emptyConv = Conv
  { _mapToInt = M.empty,
    _mapFromInt = I.empty,
    _nextInt = 0
  }

--fromInt :: Conv a -> Int -> Maybe a
--fromInt c i = I.lookup i (mapFromInt c) 
--
--toInt :: Ord a => Conv a -> a -> Maybe Int
--toInt c a = M.lookup a (mapToInt c) 
--
