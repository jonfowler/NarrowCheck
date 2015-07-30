module Reach.Parser.Combinator where

import Data.Maybe
import Control.Arrow
import Control.Applicative

data Parse t a = Parse {runParse :: ([t] -> Maybe (a , [t]))}

instance Functor (Parse t) where
  fmap f (Parse p) = Parse $ fmap (first f) . p

instance Applicative (Parse t) where
  pure a = Parse (\s -> Just (a , s))
  Parse p <*> Parse p' = Parse $ \s ->
    p s >>= \(f , s') -> fmap (first f) (p' s')

instance Monad (Parse t) where
  return = pure
  Parse p >>= m = Parse $ \s ->
    p s >>= \(a , s') -> runParse (m a) s'

instance Alternative (Parse t) where
  empty = Parse (\s -> Nothing)
  Parse p <|> Parse p' = Parse $ \s ->
    p s <|> p' s

char :: Char -> Parse Char () 
char a = Parse $ \s -> case s of
  [] -> Nothing
  (a' : s') -> if a == a' then Just (() , s') else Nothing


  
