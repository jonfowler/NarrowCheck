module Reach.Parser.Conv where


import Data.Generics.Uniplate.Data

import Reach.Syntax
import Reach.Env
import qualified Reach.Parser.ParseSyntax as P
import Reach.Parser.Parser

import qualified Data.IntMap as I
import Data.IntMap (IntMap)
import qualified Data.Map as M
import Data.Map (Map)

import Data.Char
import Data.Maybe

conv :: [P.Def] -> Defs
conv = undefined

funNames :: [P.Def] -> Conv String
funNames ds = let fns = [a | (P.Def a _ _) <-ds ] in
  foldl (\c a -> snd $ addVal a c) emptyConv fns

toEnv :: Defs -> Env
toEnv = undefined

constrNames :: [P.Def] -> [String]
constrNames e = [n | P.Alt n _ _ <- universeBi e] 
  ++ [n | P.Con n _ <- universeBi e, isUpper $ head n ]

data Convs = Convs {
  funs :: Conv String,
  cons :: Conv String,
  locals :: Conv String
  } 

conC :: Convs -> String -> Maybe ConID
conC co cid = do 
  newcid <- (toInt (cons co) cid)
  return $ ConID newcid cid

funC :: Convs -> String -> Maybe FunID
funC co  = toInt (funs co) 

localC :: Convs -> String -> Maybe VarID
localC co = toInt (locals co)

expConv :: Convs -> P.Exp -> Exp
expConv m (P.Con c es) = fromJust $ do 
  a <- conC m c
  return $ Con a (map (expConv m) es)
expConv m (P.Ap e []) = expConv m e
expConv m (P.Ap (P.Var v) es) = fromJust $ do
  fid <- funC m v
  return $ Ap fid (map (expConv m) es)
expConv m (P.Var v) = case conC m v of
  Just a -> Con a []
  Nothing -> case funC m v of
    Just a -> Ap a []
    Nothing -> case localC m v of
      Just a -> Var a
      Nothing -> error "Variable must be local, fun or constr"
expConv m (P.Case e as) = Case (expConv m e) (map (altConv m) as)

altConv :: Convs -> P.Alt -> Alt
altConv c (P.Alt vid newvs    = 
      

--expConv :: Map String Int -> P.Exp -> Exp
--expConv m (P.Ap e [])   = expConv m e 
--expConv m (P.Ap e1 [e2])  = Ap (expConv m e1) (expConv m e2)
--expConv m (P.Ap e1 es)  = let (e2:es2) = reverse es 
--  in Ap (expConv m $ P.Ap e1 (reverse es2)) (expConv m e2)
--  
--expConv m (P.Var v) = Var (VarID (m M.! v) v)



data Conv a = Conv
  { mapToInt :: Map a Int
  , mapFromInt ::  IntMap a
  , nextInt :: Int
  }

emptyConv :: Conv a
emptyConv = Conv
  { mapToInt = M.empty,
    mapFromInt = I.empty,
    nextInt = 0
  }

fromInt :: Conv a -> Int -> Maybe a
fromInt c i = I.lookup i (mapFromInt c) 

toInt :: Ord a => Conv a -> a -> Maybe Int
toInt c a = M.lookup a (mapToInt c) 

addVal :: Ord a =>  a -> Conv a -> (Int, Conv a)
addVal a c = case M.lookup a (mapToInt c) of
  Just i -> (i, c)
  Nothing -> let i = nextInt c in
    (i, c { mapToInt = M.insert a i (mapToInt c)
          , mapFromInt = I.insert i a (mapFromInt c)
          , nextInt = i + 1 })
                           



