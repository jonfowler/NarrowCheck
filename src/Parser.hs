module Parser where

import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.Token

import Syntax

import Control.Monad
import Control.Monad.Identity

type ParsecM a = ParsecT String () Identity a

var :: ParsecM VarID
var = do
  a <- letter 
  b <- many alphaNum
  return $ VarID 0 (a:b)


