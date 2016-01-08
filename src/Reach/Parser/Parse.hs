module Reach.Parser.Parse where

import Control.Applicative
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Trifecta.Result
import Text.Trifecta.Combinators
import Text.Trifecta.Delta
import qualified Text.Trifecta.Parser as T

import Reach.Parser.Tokens
import Control.Lens

import Debug.Trace       

data Con a = Con {_conName :: ConId, _conArgs :: [a]} deriving (Show)
makeLenses ''Con

data Expr = Case Expr [Alt]
          | App Expr Expr
          | Parens Expr
          | ConE ConId [Expr]
          | ConHole
          | Op Expr OpId Expr
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

parseModuleHead :: Parser [String]            
parseModuleHead = try (top "module") >> parseModuleName <* lexeme (string "where")

parseImport :: Parser [String]
parseImport = try (top "import") >>
              strictIndent >>
              parseModuleName
              

parseModuleName :: Parser [String]
parseModuleName = lexeme (sepBy1 ((:) <$> upper <*> many alphaNum) (char '.'))

parseCon :: Parser a -> Parser (Con a)
parseCon p = Con <$> parseConId <*> many (strictIndent >> p)
      
parseTypeDef :: Parser TypeDef
parseTypeDef = sameIndent >> TypeDef <$> parseVarId <* res "::" <*> parseType

parseType :: Parser Type
parseType = foldr1 (:->) <$> sepBy1 (parseInnerType) (res "->")

parseInnerType :: Parser Type
parseInnerType = strictIndent >> (between (res "(") (res ")") parseType
               <|> Type <$> parseTypeId)


parseDefOp :: Parser (Expr -> Def)
parseDefOp = do
  v <- parseVarId
  strictIndent
  o <- parseOpId 
  strictIndent
  v' <- parseVarId
  return (Def o [v,v'])
  

parseDef = do
  sameIndent
  d <- try parseDefOp <|> try (Def <$> parseVarId <*> (many (strictIndent >> parseVarId)))
  e <-  res "=" *> parseExpr
  return (d e)

parseExpr :: Parser Expr
parseExpr = parseCase <|> parseOp
                 
parseOp :: Parser Expr                 
parseOp = toOp <$> parseApp <*> optional (do
                                          strictIndent
                                          o <- try parseOpId
                                          e <- parseExpr
                                          return (o , e))

          
toOp :: Expr -> Maybe (String , Expr) -> Expr         
toOp e = maybe e (\(s,e') -> Op e s e')  

parseApp :: Parser Expr
parseApp = toApp <$> some parseInnerExpr  --parseInnerExp

parseInnerExpr :: Parser Expr
parseInnerExpr = strictIndent >> (  try (Var <$> parseVarId)
                               <|> try (ConE <$> parseConId <*> pure [])
                               <|> parseParens )

parseParens :: Parser Expr
parseParens = Parens <$> between (res "(") (res ")") parseExpr

parseAlt :: Parser Alt
parseAlt = Alt <$> try (parseCon parseVarId)
               <*> (res "->" *> parseExpr)


parseCase :: Parser Expr
parseCase = Case <$> (try (res "case") *> parseExpr)
                 <*> (res "of" *> block parseAlt)

toApp :: [Expr] -> Expr
toApp = go . reverse
  where go [e] = e
        go (e : es) = App (go es) e


