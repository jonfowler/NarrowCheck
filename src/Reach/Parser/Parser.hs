{-# LANGUAGE FlexibleContexts #-}

module Parser where

import Text.Parsec
import Text.Parsec.Language
import qualified Text.Parsec.Token as P

import ParseSyntax
import IndentParser

import Control.Monad
import Control.Monad.Identity
import Control.Monad.State

import System.IO.Unsafe

--myParse parser  input = runIndent "" $
--  runParserT parser () "" input

parserI = parseI


myTokens :: (Stream String m t) => GenLanguageDef String u m
myTokens = P.LanguageDef 
  { P.commentStart   = "{-"
  , P.commentEnd     = "-}"
  , P.commentLine    = "--"
  , P.nestedComments = True
  , P.identStart     = letter
  , P.identLetter	 = alphaNum <|> oneOf "_'"
  , P.opStart	 = P.opLetter myTokens 
  , P.opLetter	 = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , P.reservedOpNames= ["=", "->"]
  , P.reservedNames  = ["case", "of"]
  , P.caseSensitive  = True
  }

caseExpr :: ParsecI Exp
caseExpr = do
  reserved "case"
  subj <- basicExpr
  reserved "of"
  alts <- block altPatt 
  return $ Case subj alts 

readU = unsafePerformIO . readFile

altPatt :: ParsecI Alt
altPatt = do
  (c : vs) <- many1 identifier
  reserved "->" 
  e <- basicExpr
  return (Alt c vs e)



--Alt (VarID 0 "") patt
  
basicExpr :: ParsecI Exp
basicExpr = do 
  v <- identifier 
  return $ Var v

lexer = P.makeTokenParser myTokens

parens = P.parens lexer
braces = P.braces lexer
identifier = P.identifier lexer

reserved :: String -> ParsecI ()
reserved = P.reserved lexer
