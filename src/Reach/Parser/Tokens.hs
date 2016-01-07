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
    lexeme,
    whitespace,
    notReserved
    ) where

import Control.Applicative
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Trifecta.Result
import Text.Trifecta.Combinators
import Text.Trifecta.Delta
import qualified Text.Trifecta.Parser as T

import Reach.Parser.Indent
import Control.Monad

type ConId = String 
type VarId = String 
type TypeId = String

reservedT :: [String]
reservedT = ["case", "of", "data", "import", "let", "in", "module", "where"]

reservedOps :: [String]
reservedOps = ["->", "=", "(", ")", "::", "|","--","{-","-}"]

notReserved :: Parser String 
notReserved = do
  a <- (:) <$> lower <*> many alphaNum
  if a `elem` reservedT
    then unexpected $ "unexpected reserved word: " ++ a
    else return a

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace 

--word :: String -> Parser () 
--word = foldr ((*>) . char) (return ()) 

notWord :: String -> Parser ()
notWord [] = return ()

reserved :: String -> Parser String
reserved x = lexeme (string x)

parseVarId :: Parser VarId 
parseVarId = lexeme $ notReserved 

parseConId :: Parser ConId 
parseConId = lexeme $ ((:) <$> upper <*> many alphaNum) 

parseTypeId :: Parser TypeId 
parseTypeId = lexeme $ ((:) <$> upper <*> many alphaNum)

whitespace :: Parser ()
whitespace = skipMany (void (oneOf " \n") <|> void comment <|> void commentBlock)

comment :: Parser String 
comment = try (string "--") >> many (notChar '\n')

commentBlock :: Parser String 
commentBlock = try (string "{-") >> manyTill anyChar (try (string "-}"))

res :: String -> Parser String 
res a = strictIndent >> reserved a
  
top :: String -> Parser String 
top a = sameIndent >> reserved a
