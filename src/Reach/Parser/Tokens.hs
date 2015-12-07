{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Reach.Parser.Tokens
  ( module Reach.Parser.Indent,
    ConId,
    VarId,
    TypeId,
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

type ConId = String 
type VarId = String 
type TypeId = String

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
parseVarId = lexeme $ ((:) <$> lower <*> many alphaNum) >>= notReserved

parseConId :: Parser ConId 
parseConId = lexeme $ ((:) <$> upper <*> many alphaNum) >>= notReserved

parseTypeId :: Parser TypeId 
parseTypeId = lexeme $ ((:) <$> upper <*> many alphaNum) >>= notReserved

whitespace :: Parser ()
whitespace = void . many . oneOf $ " \n"

res :: String -> Parser ()
res a = strictIndent >> reserved a
  
top :: String -> Parser ()
top a = sameIndent >> reserved a
