module IndentParser where

import Text.Parsec
import Text.Parsec.Pos
import Text.Parsec.Token
import Control.Monad.Identity

data ParseState = ParseState
  {
  indentP :: Int
  }

type ParsecI a = ParsecT String ParseState Identity a

beginState :: ParseState 
beginState = ParseState {indentP = 1}

parseI :: ParsecI a -> String -> Either ParseError  a
parseI p input = runIdentity $ runParserT p beginState "" input

getIndent :: ParsecI Int
getIndent = fmap indentP getState  

putIndent :: Int -> ParsecI ()
putIndent i = modifyIndent (const i) 

modifyIndent :: (Int -> Int) -> ParsecI () 
modifyIndent f = do
  s <- getState
  putState (s {indentP = f $ indentP s})

newIndentLevel :: ParsecI ()
newIndentLevel = do
  a <- getSourceColumn
  modifyIndent (max a . (+1))

withNewIndent :: ParsecI a -> ParsecI a
withNewIndent p = do
  i <- getIndent
  newIndentLevel
  j <- getIndent
  a <-  p
  putIndent i
  return a

block :: ParsecI a -> ParsecI [a]
block p = withNewIndent $ many (checkIndent >> p)
 
checkIndent :: ParsecI ()
checkIndent = do
  i <- getIndent
  j <- getSourceColumn
  unless (i==j) $ parserFail "indentation fail"
  
getSourceColumn :: ParsecI Int
getSourceColumn = fmap sourceColumn getPosition
