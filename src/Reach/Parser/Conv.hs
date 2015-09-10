{-# LANGUAGE TemplateHaskell #-}

module Reach.Parser.Conv where

import qualified Data.IntMap as I
import Data.IntMap (IntMap)
import qualified Data.Map as M
import Data.Map (Map)

import Control.Lens

import qualified Reach.Parser.Module as S
import Reach.Eval.Exp


data Conv a = Conv
  { _mapToInt :: Map a Int
  , _mapFromInt ::  IntMap a
  , _nextInt :: Int
  }

makeLenses ''Conv

data Convert = Convert {
  _convertDef :: Conv S.VarId,
  _convertCon :: Conv S.ConId,
  _convertLocals :: Conv S.VarId
                       }

makeLenses ''Convert

data ConvertM a = ConvertM {runConvert :: (Convert -> (a , Int))}

localVars :: (Conv S.VarId -> Conv S.VarId) -> ConvertM a -> ConvertM a
localVars f (ConvertM m) = ConvertM $ m . (convertLocals %~ f)


instance Functor ConvertM where
  fmap f (ConvertM r) = ConvertM (r & mapped . _1 %~ f)

instance Applicative ConvertM where
  pure a = ConvertM $ \c -> (a , c ^. convertLocals . nextInt)
  ConvertM r <*> ConvertM r' = ConvertM (\c -> let (f , m) = r c
                                                   (a , n) = r' c
                                               in (f a, max m n))

instance Monad ConvertM where
  return = pure
  ConvertM m >>= f = ConvertM $ \c -> let (a , p) = m c 
                                      in runConvert (f a) c & _2 %~ max p


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
--addVal :: Ord a =>  a -> Conv a -> (Int, Conv a)
--addVal a c = case toInt c a of
--  Just i -> (i, c)
--  Nothing -> let i = nextInt c in
--    (i, c { mapToInt = M.insert a i (mapToInt c)
--          , mapFromInt = I.insert i a (mapFromInt c)
--          , nextInt = i + 1 })
