{-# LANGUAGE FlexibleContexts #-}

module Reach.Parser.Parser where

import Text.Parsec
import Text.Parsec.Language
import qualified Text.Parsec.Token as P

import Reach.Parser.ParseSyntax
import Reach.Parser.IndentParser
import Reach.Parser.LanguageDef

import Control.Monad
import Control.Monad.Identity
import Control.Monad.State

import System.IO.Unsafe

--myParse parser  input = runIndent "" $
--  runParserT parser () "" input
--
--

parserI = parseI

readU = unsafePerformIO . readFile

def :: ParsecI Def
def = do
  (v : vs) <- many1 identifier
  reserved "="
  reserved "{"
  e <- expr
  reserved "}"
  return (Def v vs e)

parseFile :: ParsecI [Def]
parseFile =  whiteSpace >> many1 def

expr :: ParsecI Exp
expr =  caseExpr
    <|> lamExpr
    <|> appExpr    
 
innerExpr :: ParsecI Exp
innerExpr = parens expr 
         <|> varExpr

varExpr :: ParsecI Exp
varExpr = fmap Var identifier 

lamExpr :: ParsecI Exp
lamExpr = do
  reserved "\\"
  vs <- many1 identifier
  reserved "->"
  e <- expr 
  return (Lam vs e) 

caseExpr :: ParsecI Exp
caseExpr = do
  reserved "case"
  subj <- expr
  reserved "of"
  alts <- block altPatt 
  return $ Case subj alts 

appExpr :: ParsecI Exp
appExpr = do 
  (e : es) <- many1 innerExpr 
  return (Ap e es)

altPatt :: ParsecI Alt
altPatt = do
  (c : vs) <- many1 identifier
  reserved "->" 
  e <- expr 
  reserved ";"
  return (Alt c vs e)

