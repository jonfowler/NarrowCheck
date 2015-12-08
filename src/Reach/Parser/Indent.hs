module Reach.Parser.Indent (
  runParse,
  block,
  checkIndent,
  localIndent,
  strictIndent,
  sameIndent,
  looseIndent,
  getColumn,
  module Text.Parsec,
  Parser
  ) where

import Text.Parsec hiding (parseTest)
import Control.Monad.Identity

data ParseState = ParseState {indentLevel :: Int }
type Parser a = ParsecT String ParseState Identity a
type ParserT m a = ParsecT String ParseState m a

runParse :: Parser a -> String -> Either ParseError a
runParse p = runParser p startState ""  

startState :: ParseState
startState = ParseState {indentLevel = 1}

writeIndent :: Int -> ParseState -> ParseState
writeIndent n (ParseState _) = ParseState n

getIndent :: Parser Int
getIndent = indentLevel <$> getState

putIndent :: Int -> Parser ()
putIndent n = modifyState (writeIndent n)

localIndent :: Int -> Parser a -> Parser a
localIndent n p = do
  m <- getIndent
  putIndent n
  a <- p
  putIndent m
  return a

getColumn :: Parser Int
getColumn = sourceColumn <$> getPosition 

guardParse :: String -> Bool -> Parser ()
guardParse err b = if b then return () else unexpected err 

checkIndent :: (Int -> Int -> Bool) -> Parser ()
checkIndent f = f <$> getIndent <*> getColumn >>= guardParse "Parse error: possibly indentation" 

strictIndent :: Parser ()
strictIndent = checkIndent (<) 

looseIndent :: Parser ()
looseIndent = checkIndent (\ind col -> col /= 1 && ind <= col) 

sameIndent :: Parser ()
sameIndent = checkIndent (==) 

block :: Parser a -> Parser [a]
block p = do
  strictIndent
  n <- getColumn
  localIndent n (many $ sameIndent >> p)


