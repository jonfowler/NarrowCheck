module Reach.Parser.Parse where

import Reach.Parser.Tokens
import Control.Lens

data Con a = Con {_conName :: ConId, _conArgs :: [a]} deriving (Show)
makeLenses ''Con

data Exp = Case Exp [Alt]
         | App Exp Exp
         | Parens Exp
         | ConE ConId
         | Var VarId deriving (Show)

data Alt = Alt {_altPattern :: Con VarId, _altBody :: Exp} deriving (Show)
makeLenses ''Alt

data Def = Def {_defName :: VarId, _defArgs :: [VarId], _defBody :: Exp} deriving (Show)
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

parseExp :: Parser Exp
parseExp = (parseCase <|> parseApp)

parseDef = try (Def <$>
    (sameIndent >> parseVarId) <*>
    (many (strictIndent >> parseVarId) <* res "=")) <*>
    parseExp

parseInnerExp :: Parser Exp
parseInnerExp = strictIndent >> (  try (Var <$> parseVarId)
                               <|> try (ConE <$> parseConId)
                               <|> parseParens )

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
        go (e : es) = App (go es) e


