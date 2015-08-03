{-# LANGUAGE DeriveFunctor #-}

module Reach.Parser.Combinator where

import Data.Maybe
import Control.Arrow
import Control.Applicative
import Control.Monad

data Parse t a = Parse {runParse :: ([t] -> Maybe (a , [t]))}

data Parser t a
  = Done a 
  | Fail 
  | Cont (t -> Parser t a)
  deriving (Functor)
                  
                  

instance Functor (Parse t) where
  fmap f (Parse p) = Parse $ fmap (first f) . p

instance Applicative (Parse t) where
  pure a = Parse (\s -> Just (a , s))
  Parse p <*> Parse p' = Parse $ \s ->
    p s >>= \(f , s') -> fmap (first f) (p' s')

instance Applicative (Parser t) where
  pure = Done
  Done f <*> a = f <$> a
  Fail <*> _ = Fail
  Cont p <*> a = Cont (\t -> p t <*> a)

instance Monad (Parse t) where
  return = pure
  Parse p >>= m = Parse $ \s ->
    p s >>= \(a , s') -> runParse (m a) s'

instance Monad (Parser t) where
  return = pure 
  Done a >>= m = m a 
  Fail >>= _ = Fail
  Cont p >>= f = Cont (p >=> f)

instance Alternative (Parser t) where
  empty = Fail
  Done a <|> _ = Done a
  Fail <|> p = p
  Cont p <|> Cont q = Cont (\t -> p t <|> q t)
  Cont p <|> Fail = Cont p
  Cont p <|> Done a = Cont (\t -> p t <|> Done a)

instance Alternative (Parse t) where
  empty = Parse (\s -> Nothing)
  Parse p <|> Parse p' = Parse $ \s ->
    p s <|> p' s

char :: Char -> Parse Char () 
char a = Parse $ \s -> case s of
  [] -> Nothing
  (a' : s') -> if a == a' then Just (() , s') else Nothing

token :: Eq a => a -> Parser a ()
token a = Cont $ \t -> case t == a of
  True -> Done () 
  False -> Fail


test a b = case a of
    Nothing -> Nothing
    Just c -> case c of
      Nothing -> Nothing <|>
       Nothing
      Just d -> r 
      where r = undefined
