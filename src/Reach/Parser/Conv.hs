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

import Control.Monad.Except
import Control.Applicative
import Data.Monoid

import Data.Char
import Data.Maybe
import Data.Either

conv :: [P.Def] -> [Fun]
conv = undefined

funNames :: [P.Def] -> Conv String
funNames ds = let fns = [a | (P.Def a _ _) <-ds ] in
  snd $ addVals fns emptyConv 


constrNames :: [P.Def] -> [String]
constrNames e = [n | P.Alt n _ _ <- universeBi e] 
  ++ [n | P.Con n _ <- universeBi e, isUpper $ head n ]

data Convs = Convs {
  funs :: Conv String,
  cons :: Conv String,
  locals :: Conv String
  } 

conC :: Monad m => Convs -> String -> ExceptT String m ConID
conC co cid = fromMaybe (throwError "No constructor name") $ do
  a <- toInt (cons co) cid
  return . return $ ConID a cid

funC :: Monad m => Convs -> String -> ExceptT String m FunID
funC co  = maybe (throwError "No function name") return 
           . toInt (funs co)

localC :: Monad m => Convs -> String -> ExceptT String m VarID
localC co = maybe (throwError "No local name") return 
           . toInt (locals co)

data Moni b a = Moni b a

editMoni :: b -> Moni c a -> Moni b a
editMoni b (Moni _ a) = Moni b a


instance Monoid b => Monad (Moni b) where
  return = Moni mempty 
  Moni m1 a >>= f = let Moni m2 b = f a
    in Moni (m1 `mappend` m2) b

instance Functor (Moni b) where
  fmap f (Moni b a) = Moni b $ f a

instance Monoid Int  where
  mempty = 0
  mappend = max

expConv :: Convs -> P.Exp -> ExceptT String (Moni Int) Exp
expConv m (P.Con c es) =  do 
  a <- conC m c
  newes <- mapM (expConv m) es
  return $ Con a newes
expConv m (P.Ap e []) = expConv m e
expConv m (P.Ap (P.Var v) es) =  do
  fid <- funC m v
  newes <- mapM (expConv m) es
  return $ Ap fid newes 
expConv m (P.Var v) = do
  lval <- lift $ Moni (nextInt $ locals m) v
  ((\a -> Con a []) <$> conC m lval)
    <|> ((\a -> Ap a [])  <$> funC m lval)
    <|> (Var <$> localC m lval)
expConv m (P.Case e as) = do
  a <- expConv m e
  newas <- mapM (altConv m) as
  return $ Case a newas

altConv :: Convs -> P.Alt -> ExceptT String (Moni Int) Alt
altConv c (P.Alt vid vs e) = do
  let (newvs, newls) = addVals vs $ locals c  
      newc = c {locals = newls}
  cid <- conC c vid
  newe <- expConv newc e
  return $ Alt cid newvs newe
  
defToFun :: Conv String -> Conv String -> P.Def -> Either String Fun
defToFun fs cs (P.Def n vs e) = let 
  (newvs,ls) = addVals vs emptyConv
  c = Convs{ cons = cs, funs = fs, locals =ls}
  Moni i a = runExceptT (expConv c e)
  in fmap (\bod -> Fun {
        body = bod,
        fid = fromRight $ runExcept (funC c n),
        name = n,
        args = newvs,
        varNum = i}) a

fromRight (Right a) = a

addVals :: Ord a => [a] -> Conv a -> ([Int], Conv a)
addVals [] c = ([], c)
addVals (a:as) c = let (i, c') = addVal a c
                       (is, c'') = addVals as c'
  in (i:is, c'')




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
                           



