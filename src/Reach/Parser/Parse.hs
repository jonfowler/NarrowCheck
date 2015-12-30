module Reach.Parser.Parse where

import Reach.Parser.Tokens
import Control.Lens

data Con a = Con {_conName :: ConId, _conArgs :: [a]} deriving (Show)
makeLenses ''Con

data Expr = Case Expr [Alt]
          | App Expr Expr
          | Parens Expr
          | ConE ConId [Expr]
          | ConHole
          | Var VarId deriving (Show)

data Alt = Alt {_altPattern :: Con VarId, _altBody :: Expr} deriving (Show)
makeLenses ''Alt

data Def = Def {_defName :: VarId, _defArgs :: [VarId], _defBody :: Expr} deriving (Show)
makeLenses ''Def

data Type = Type :-> Type
          | Type TypeId deriving (Show)

data Data = Data {_dataName :: TypeId, _dataCon :: [Con Type]} deriving (Show)
makeLenses ''Data

data TypeDef = TypeDef {_typeDefName :: VarId, _typeDefType :: Type} deriving (Show)
makeLenses ''TypeDef

parseData :: Parser Data
parseData = try (top "data") >> (Data
      <$> (strictIndent >> parseTypeId)
      <*> (res "=" >> sepBy1 (strictIndent >> parseCon parseType) (res "|")))

parseCon :: Parser a -> Parser (Con a)
parseCon p = Con <$> parseConId <*> many (strictIndent >> p)
      
parseTypeDef :: Parser TypeDef
parseTypeDef = sameIndent >> TypeDef <$> parseVarId <* res "::" <*> parseType

parseType :: Parser Type
parseType = foldr1 (:->) <$> sepBy1 (parseInnerType) (res "->")

parseInnerType :: Parser Type
parseInnerType = strictIndent >> (between (res "(") (res ")") parseType
               <|> Type <$> parseTypeId)

parseExp :: Parser Expr
parseExp = (parseCase <|> parseApp)

parseDef = try (Def <$>
    (sameIndent >> parseVarId) <*>
    (many (strictIndent >> parseVarId) <* res "=")) <*>
    parseExp

parseInnerExp :: Parser Expr
parseInnerExp = strictIndent >> (  try (Var <$> parseVarId)
                               <|> try (ConE <$> parseConId <*> pure [])
                               <|> parseParens )

parseParens :: Parser Expr
parseParens = Parens <$> between (res "(") (res ")") parseExp

parseAlt :: Parser Alt
parseAlt = Alt <$> try (parseCon parseVarId)
               <*> (res "->" *> parseExp)


parseCase :: Parser Expr
parseCase = Case <$> (try (res "case") *> parseExp)
                 <*> (res "of" *> block parseAlt)

parseApp :: Parser Expr
parseApp = toApp <$> many1 parseInnerExp  --parseInnerExp

toApp :: [Expr] -> Expr
toApp = go . reverse
  where go [e] = e
        go (e : es) = App (go es) e


