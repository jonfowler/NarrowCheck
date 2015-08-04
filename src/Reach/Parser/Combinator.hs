{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Reach.Parser.Combinator where

import Data.Maybe
import Control.Arrow
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Except

data Parser t a
  = Done a
  | Fail String
  | Read (t -> Parser t a)
  | Choice (Parser t a) (Parser t a) 
  | Try (Parser t a)
  deriving (Functor)
                  
pmap :: (a -> Parser t b) -> (Parser t a -> Parser t b) ->
          Parser t a -> Parser t b
pmap f g (Done a) = f a
pmap f g (Fail e) = Fail e
pmap f g (Read p) = Read (g <$> p)
pmap f g (Choice p q) = Choice (g p) (g q)
pmap f g (Try p) = Try (g p)

instance Applicative (Parser t) where
  pure = Done
  p <*> q = pmap (<$> q) (<*> q) p

instance Monad (Parser t) where
  return = Done
  p >>= q = pmap q (>>= q) p

instance Alternative (Parser t) where
  empty = Fail ""
  p <|> q = Choice p q

type ParseMonad t a = ExceptT String (State [t]) a  --ParseMonad (m a) deriving (Functor)

runParser :: Parser t a -> [t] -> (Either String a , [t])
runParser p ts = runState (runExceptT (parseMonad p)) ts

parseMonad :: Parser t a -> ParseMonad t a
parseMonad (Done a) = return a
parseMonad (Fail e) = throwError e
parseMonad (Read f) = do
  ts <- get
  case ts of
    [] -> throwError "EOF"
    (t : ts') -> case f t of
      Fail e -> throwError e 
      p -> put ts' >> parseMonad (f t)
parseMonad (Choice p q) = catchError (parseMonad p) (const (parseMonad q))
parseMonad (Try p) = do
  ts <- get
  catchError (parseMonad p) (\e -> put ts >> throwError e)

token :: (Show t, Eq t) => t -> Parser t ()
token t = Read $ \t' -> case t == t' of
  False -> Fail $ "Expecting: " ++ show t ++ " Found: " ++ show t'
  True -> Done ()

word :: (Show t, Eq t) => [t] -> Parser t ()
word [] = Done ()
word (t : ts) = Read $ \t' -> case t == t' of
  False -> Fail $ "Expecting: " ++ show t ++ " Found: " ++ show t'
  True -> word ts

test a b = case a of
    Nothing -> Nothing
    Just c -> case c of
      Nothing -> Nothing <|>
       Nothing
      Just d -> r 
      where r = undefined

test2 a b = case a of {Nothing -> b; Just a -> b }
