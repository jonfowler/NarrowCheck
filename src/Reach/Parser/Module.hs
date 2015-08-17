module Reach.Parser.Module where

import Reach.Parser.Tokens
import Reach.Parser.Parse

data Module = Module [Data] [Def] [TypeDef] deriving (Show)

emptyModule :: Module
emptyModule = Module [] [] []

addData :: Data -> Module -> Module
addData d (Module ds defs tds) = Module (d : ds) defs tds

addDef :: Def -> Module -> Module
addDef d (Module ds defs tds) = Module ds (d : defs) tds

addTypeDef :: TypeDef -> Module -> Module
addTypeDef td (Module ds defs tds) = Module ds defs (td : tds)

parseModule :: Parser Module
parseModule = whitespace >> foldl (flip ($)) emptyModule <$> many (
                   addData <$> parseData
               <|> addDef <$> parseDef
               <|> addTypeDef <$> parseTypeDef)

