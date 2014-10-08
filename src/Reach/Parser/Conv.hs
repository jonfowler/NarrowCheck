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


conv :: [P.Def] -> Defs
conv = undefined

funNames :: [P.Def] -> Conv String
funNames ds = let fns = [a | (P.Def a _ _) <-ds ] in
  foldl (\c a -> snd $ addVal a c) emptyConv fns

toEnv :: Defs -> Env
toEnv = undefined

constrNames :: [P.Def] -> [String]
constrNames e = [n | P.Alt n _ _ <- universeBi e] 

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

fromInt :: Conv a -> Int -> a
fromInt c i = mapFromInt c I.! i

toInt :: Ord a => Conv a -> a -> Int
toInt c a = mapToInt c M.! a

addVal :: Ord a =>  a -> Conv a -> (Int, Conv a)
addVal a c = case M.lookup a (mapToInt c) of
  Just i -> (i, c)
  Nothing -> let i = nextInt c in
    (i, c { mapToInt = M.insert a i (mapToInt c)
          , mapFromInt = I.insert i a (mapFromInt c)
          , nextInt = i + 1 })
                           



