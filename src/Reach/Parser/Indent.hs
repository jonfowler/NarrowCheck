module Reach.Parser.Indent (
  runParse,
  block,
  checkIndent,
  localIndent,
  strictIndent,
  thisstrictIndent,
  sameIndent,
  getColumn,
  Parser
  ) where

--import Text.Parsec hiding (parseTest)
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Trifecta.Result
import Text.Trifecta.Combinators
import Text.Trifecta.Delta
import qualified Text.Trifecta.Parser as T
      
import Data.Int
import Control.Monad.State
import Control.Monad.Identity
import Reach.Lens

data ParseState = ParseState {_indentLevel :: Int64 }
makeLenses ''ParseState
                  
type Parser a = StateT ParseState T.Parser a   --ParsecT String ParseState Identity a
--type ParserT m a =    ParsecT String ParseState m a

runParse :: Parser a -> String -> Result a
runParse p = fmap fst . T.parseString (runStateT p startState) mempty

startState :: ParseState
startState = ParseState {_indentLevel = 0}

--writeIndent :: Int -> ParseState -> ParseState
--writeIndent n (ParseState _) = ParseState n

--getIndent :: Parser Int
--getIndent = indentLevel <$> getState

--putIndent :: Int -> Parser ()
--putIndent n = modifyState (writeIndent n)

localIndent :: Int64 -> Parser a -> Parser a
localIndent n p = do
  m <- use indentLevel
  indentLevel .= n
  a <- p
  indentLevel .= m
  return a

getColumn :: Parser Int64
getColumn = column <$> position 

guardParse :: String -> Bool -> Parser ()
guardParse err b = if b then return () else lift . raiseErr $ failed err 

thisstrictIndent :: Parser ()
thisstrictIndent = do
  i <- use indentLevel
  c <- getColumn
  if i < c 
    then return ()
    else lift . raiseErr $ failed "this strictindent"



checkIndent :: (Int64 -> Int64 -> Bool) -> Parser ()
checkIndent f = f <$> use indentLevel <*> getColumn >>= guardParse "Parse error: possibly indentation" 

strictIndent :: Parser ()
strictIndent = checkIndent (<) 

sameIndent :: Parser ()
sameIndent = checkIndent (==) 

block :: Parser a -> Parser [a]
block p = do
  strictIndent
  n <- getColumn
  localIndent n (many $ sameIndent >> p)


