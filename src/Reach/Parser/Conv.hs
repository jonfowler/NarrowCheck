module Reach.Parser.Conv where

import Data.Generics.Uniplate.Data

import Reach.Syntax
import qualified Reach.Parser.ParseSyntax as P
import Reach.Parser.Parser

import qualified Data.IntMap as I
import Data.IntMap (IntMap)
import qualified Data.Map as M
import Data.Map (Map)

import Control.Monad.Writer
import Control.Applicative
import Data.Monoid

import Data.Char
import Data.Maybe
import Data.Either

conv :: (Monad m, Functor m) => [P.Def] -> ExceptT String m [Func] 
conv ds = let
  fs = funNames ds
  cs = constrNames ds
  in mapM (defToFun fs cs) ds

funNames :: [P.Def] -> Conv String
funNames ds = let fns = [a | (P.Def a _ _) <-ds ] in
  snd $ addVals fns emptyConv 

constrNames :: [P.Def] -> Conv String 
constrNames e = let 
  cs = [n | P.Alt n _ _ <- universeBi e] 
       ++ [n | P.Con n _ <- universeBi e]
  in snd $ addVals cs emptyConv

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
funC co  = maybe (throwError "No function name") (return . FunID) 
           . toInt (funs co)

localC :: Monad m => Convs -> String -> ExceptT String m VarID
localC co = maybe (throwError "No local name") (return . VarID)
           . toInt (locals co)

instance Monoid Int  where
  mempty = 0
  mappend = max


expConv :: (Functor m, Monad m) => Convs -> P.Exp -> WriterT Int (ExceptT String m) Exp
expConv m (P.Con c es) =  do 
  a <- lift $ conC m c
  newes <- mapM (expConv m) es
  return $ Con a newes
expConv m (P.Ap e []) = expConv m e
expConv m (P.Ap e es) =  do
  newe <- expConv m e
  newes <- mapM (expConv m) es
  return $ Ap newe  newes 
expConv m (P.Var v) = do
  lval <- writer (v, nextInt $ locals m )
  ((\a -> Con a []) <$> lift (conC m lval))
    <|> Fun  <$> lift (funC m lval)
    <|> Var <$> lift (localC m lval)
expConv m P.Target = writer (Target, nextInt $ locals m)
expConv m (P.Case e as) = do
  a <- expConv m e
  newas <- mapM (altConv m) as
  return $ Case a newas

altConv :: (Functor m, Monad m) => Convs -> P.Alt -> WriterT Int (ExceptT String m) Alt
altConv c (P.Alt vid vs e) = do
  let (newvs, newls) = addVals vs $ locals c  
      newc = c {locals = newls}
  cid <- lift $ conC c vid
  newe <- expConv newc e
  return $ Alt cid (map VarID newvs) newe
  
defToFun :: (Monad m, Functor m) => 
  Conv String -> Conv String -> P.Def -> ExceptT String m Func
defToFun fs cs (P.Def n vs e) = let 
  (newvs,ls) = addVals vs emptyConv
  c = Convs{ cons = cs, funs = fs, locals =ls}
  in do
    (a, i) <- runWriterT (expConv c e)
    return Func{
        body = a,
        fid = fromRight $ runExcept (funC c n),
        name = n,
        args = map VarID newvs,
        varNum = VarID i} 

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
                           



