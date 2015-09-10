{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Reach.Parser.Tokens
  ( module Reach.Parser.Indent,
    ConId,
    VarId,
    TypeId,
    conId,
    varId,
    typeId,
    parseVarId,
    parseConId,
    parseTypeId,
    res,
    top,
    reserved,
    whitespace,
    ) where

import Reach.Parser.Indent
import Control.Monad

newtype ConId = ConId String deriving (Eq, Ord)
newtype VarId = VarId String deriving (Eq, Ord)
newtype TypeId = TypeId String deriving (Eq, Ord)

conId (ConId c) = c
varId (VarId v) = v
typeId (TypeId t) = t

instance Show ConId where show (ConId a) = show a
instance Show VarId where show (VarId a) = show a
instance Show TypeId where show (TypeId a) = show a

reservedT :: [String]
reservedT = ["case", "of", "data"]

reservedOps :: [String]
reservedOps = ["->", "=", "(", ")", "::", "|"]

notReserved :: String -> Parser String 
notReserved a | any (== a) reservedT = unexpected $ "reserved word " ++ a
              | otherwise = return a 

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace 

word :: String -> Parser () 
word [] = return ()
word (a : as) = char a *> word as

reserved :: String -> Parser () 
reserved x = lexeme (word x)

parseVarId :: Parser VarId 
parseVarId = VarId <$> (lexeme $ ((:) <$> lower <*> many alphaNum) >>= notReserved)

parseConId :: Parser ConId 
parseConId = ConId <$> (lexeme $ ((:) <$> upper <*> many alphaNum) >>= notReserved)

parseTypeId :: Parser TypeId 
parseTypeId = TypeId <$> (lexeme $ ((:) <$> upper <*> many alphaNum) >>= notReserved)

whitespace :: Parser ()
whitespace = void . many . oneOf $ " \n"

res :: String -> Parser ()
res a = strictIndent >> reserved a
  
top :: String -> Parser ()
top a = sameIndent >> reserved a
