module Reach.Parser.Tokens
  ( module Reach.Parser.Indent,
    ConId,
    VarId,
    TypeId,
    OpId,
    parseVarId,
    parseConId,
    parseTypeId,
    parseOpId,
    res,
    top,
    reserved,
    lexeme,
    whitespace,
    notReserved,
    notReservedOp
    ) where

import Control.Applicative
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Trifecta.Result
import Text.Trifecta.Combinators
import Text.Trifecta.Delta
import qualified Text.Trifecta.Parser as T

import Data.Char
import Reach.Parser.PExpr
import Reach.Parser.Indent

import Control.Monad.Writer
import Control.Monad

import Debug.Trace

-- Reserved with strict indent, primarily used once in the body of an expression
res :: String -> Parser String 
res a = strictIndent >> reserved a
  
-- Reserved with same indent, primarily used for top level declarations
-- e.g. data
top :: String -> Parser String 
top a = sameIndent >> reserved a

parseVarId :: Parser VarId 
parseVarId = lexeme $ notReserved 

parseConId :: Parser ConId 
parseConId = lexeme $ ((:) <$> upper <*> many alphaNum) 

parseTypeId :: Parser TypeId 
parseTypeId = lexeme $ ((:) <$> upper <*> many alphaNum)
 

-- Reserved names
reservedT :: [String]
reservedT = ["case", "of", "data", "import", "let", "in", "module", "where"]

-- Parse a non reserved lower case word
notReserved :: Parser String 
notReserved = do
  a <- (:) <$> lower <*> many (alphaNum <|> oneOf "_'")
  if a `elem` reservedT
    then unexpected $ "unexpected reserved word: " ++ a
    else return a

-- Reserved operations
reservedOps :: [String]
reservedOps = ["->", "=", "|","--","{-","-}"]

notReservedOp :: Parser String 
notReservedOp = do
  a <- some operatorChars
  if a `elem` reservedOps
    then unexpected $ "unexpected reserved operation: " ++ a
    else return a

 
operatorChars :: Parser Char 
operatorChars = oneOf "*-+&^%$£!|<>.,/?:;@#~="

parseOpId :: Parser String
parseOpId = lexeme notReservedOp

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace 

reserved :: String -> Parser String
reserved x = lexeme (string x)

whitespace :: Parser ()
whitespace = skipMany $ void (oneOf " \n")
                     <|> void comment
                     <|> pragma
                     <|> distr
                     <|> void commentBlock

distr :: Parser ()
distr = try $ do
  string "{-#"
  some $ oneOf " \n"
  stringCase "DIST"
  some $ oneOf " \n"
  x <- ((:) <$> upper <*> many alphaNum)
  some $ oneOf " \n"
  n <- some $ digit
  some $ oneOf " \n"
  string "#-}"
  trace ("Distribution: " ++ x) $
    tell $ [Dist x (read n)]
  return () 

pragma :: Parser () 
pragma = try $ do
  string "{-#"
  some $ oneOf " \n"
  stringCase "PRAGMA"
  some $ oneOf " \n"
  stringCase "OVERLAP"
  some $ oneOf " \n"
  x <- notReserved <|> (char '('  *> notReservedOp <* char ')')
  some $ oneOf " \n"
  string "#-}"
  tell $ [Overlap x]
  return () 


charCase c = char (toUpper c) <|> char (toLower c)

stringCase s = foldr (\c m -> (:) <$> charCase c <*> m) (pure []) s

comment :: Parser String 
comment = try (string "--") >> many (notChar '\n') 

commentBlock :: Parser String 
commentBlock = try (string "{-") >> manyTill anyChar (try (string "-}"))

