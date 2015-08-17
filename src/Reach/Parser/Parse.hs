{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Reach.Parser.Parse where

import Reach.Parser.Tokens

data Con a = Con ConId [a] deriving (Show)

data Alt = Alt (Con VarId) Exp deriving (Show)

data Exp = Case Exp [Alt]
         | App Exp Exp
         | Parens Exp
         | Var VarId deriving (Show)

data Def = Def {name :: VarId, vars :: [VarId], body :: Exp} deriving (Show)

data Data = Data TypeId [Con Type] deriving (Show)

data Type = Type :->: Type
          | Type1 TypeId deriving (Show)

data TypeDef = TypeDef VarId Type deriving (Show)

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

parseData :: Parser Data
parseData = try (top "data") >> (Data
      <$> (strictIndent >> parseTypeId)
      <*> (res "=" >> sepBy1 (strictIndent >> parseCon parseType) (res "|")))

parseCon :: Parser a -> Parser (Con a)
parseCon p = Con <$> parseConId <*> many (strictIndent >> p)
      
parseTypeDef :: Parser TypeDef
parseTypeDef = sameIndent >> TypeDef <$> parseVarId <* res "::" <*> parseType

parseType :: Parser Type
parseType = foldr1 (:->:) <$> sepBy1 (parseInnerType) (res "->")

parseInnerType :: Parser Type
parseInnerType = strictIndent >> (between (res "(") (res ")") parseType
               <|> Type1 <$> parseTypeId)

parseExp :: Parser Exp
parseExp = (parseCase <|> parseApp)

parseDef = try (Def <$>
    (sameIndent >> parseVarId) <*>
    (many (strictIndent >> parseVarId) <* res "=")) <*>
    parseExp

parseInnerExp :: Parser Exp
parseInnerExp = strictIndent >> (try (Var <$> parseVarId) <|> parseParens )

parseParens :: Parser Exp
parseParens = Parens <$> between (res "(") (res ")") parseExp

parseAlt :: Parser Alt
parseAlt = Alt <$> try (parseCon parseVarId)
               <*> (res "->" *> parseExp)


parseCase :: Parser Exp
parseCase = Case <$> (try (res "case") *> parseExp)
                 <*> (res "of" *> block parseAlt)

parseApp :: Parser Exp  
parseApp = toApp <$> many1 parseInnerExp  --parseInnerExp

toApp :: [Exp] -> Exp
toApp = go . reverse
  where go [e] = e
        go (e : es) = App (toApp es) e

