{-# LANGUAGE FlexibleContexts #-}

module Reach.Parser.LanguageDef where

import Text.Parsec
import Text.Parsec.Language
import qualified Text.Parsec.Token as P

import Reach.Parser.IndentParser

import Control.Monad



myTokens :: Stream String m t => GenLanguageDef String u m
myTokens = P.LanguageDef 
  { P.commentStart   = "{-"
  , P.commentEnd     = "-}"
  , P.commentLine    = "--"
  , P.nestedComments = True
  , P.identStart     = letter
  , P.identLetter	 = alphaNum <|> oneOf "_'"
  , P.opStart	 = P.opLetter myTokens 
  , P.opLetter	 = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , P.reservedOpNames= ["=", "->",";", "\\"]
  , P.reservedNames  = ["case", "of"]
  , P.caseSensitive  = True
  }

lexer :: Stream String m t => P.GenTokenParser String u m
lexer = P.makeTokenParser myTokens

parens :: ParsecI a -> ParsecI a
parens = P.parens lexer

braces :: ParsecI a -> ParsecI a
braces = P.braces lexer

identifier :: ParsecI String
identifier = P.identifier lexer

reserved :: String -> ParsecI ()
reserved = P.reserved lexer

whiteSpace :: ParsecI ()
whiteSpace = P.whiteSpace lexer


