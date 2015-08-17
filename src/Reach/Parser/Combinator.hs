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

data ParseState t = ParseState
   { _inputState :: [t]
   }

getInput :: Monad m => ParseMonad t m [t] 
getInput = _inputState <$> get 

putInput :: Monad m => [t] -> ParseMonad t m () 
putInput ts = modify (\ParseState {_inputState = _} ->  ParseState {_inputState = ts})

type ParseMonad t m a = ExceptT String (StateT (ParseState t) m) a  --ParseMonad (m a) deriving (Functor)

runParser :: Parser t a -> [t] -> (Either String a , ParseState t)
runParser p ts = runState (runExceptT (parseMonad p)) (ParseState {_inputState = ts})

parseMonad :: Monad m => Parser t a -> ParseMonad t m a
parseMonad (Done a) = return a
parseMonad (Fail e) = throwError e
parseMonad (Read f) = do
  ts <- getInput
  case ts of
    [] -> throwError "EOF"
    (t : ts') -> case f t of
      Fail e -> throwError e 
      p -> putInput ts' >> parseMonad (f t)
parseMonad (Choice p q) = catchError (parseMonad p) (const (parseMonad q))
parseMonad (Try p) = do
  ts <- getInput
  catchError (parseMonad p) (\e -> putInput ts >> throwError e)

token :: (Show t, Eq t) => t -> Parser t ()
token t = Read $ \t' -> case t == t' of
  False -> Fail $ "Expecting: " ++ show t ++ " Found: " ++ show t'
  True -> Done ()

tokens :: (Show t, Eq t) => [t] -> Parser t ()
tokens [] = Done ()
tokens (t : ts) = Read $ \t' -> case t == t' of
  False -> Fail $ "Expecting: " ++ show t ++ " Found: " ++ show t'
  True -> tokens ts

test a b = case a of
    Nothing -> Nothing
    Just c -> case c of
      Nothing -> r 
       Nothing
      Just d -> r 
      where r = undefined 

test2 a b = case case a of
  True -> True 
  False ->
   case a of
   True -> True
   False -> False
  of 
  True -> a
  False -> b
