module Reach.Parser.Parse where

import Control.Applicative
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Trifecta.Result
import Text.Trifecta.Combinators
import Text.Trifecta.Delta
import qualified Text.Trifecta.Parser as T

import Reach.Parser.Tokens
import Reach.Parser.PExpr

import Control.Lens

--data Con a = Con {_conName :: ConId, _conArgs :: [a]} deriving (Show)
--makeLenses ''Con
--
--data Expr = Case Expr [Alt]
--          | App Expr Expr
--          | Parens Expr
--          | ConE ConId [Expr]
--          | Op Expr OpId Expr
--          | Var VarId deriving (Show)
--
--data Alt = Alt {_altPattern :: Con VarId, _altBody :: Expr} deriving (Show)
--makeLenses ''Alt
--
--data TypeDef = TypeDef {_typeDefName :: VarId, _typeDefType :: PType} deriving (Show)
--makeLenses ''TypeDef

parseData :: Parser PData
parseData = do
  try (top "data")
  strictIndent
  tid <- parseTypeId
  res "="
  ds <-sepBy1 (strictIndent >> parseCon parseType) (res "|")
  optional (res "deriving" >> many (strictIndent >> anyChar) >> whitespace)
  return (tid, ds)

parseModuleHead :: Parser [String]            
parseModuleHead = try (top "module") >> parseModuleName <* lexeme (string "where")

parseImport :: Parser (Maybe [String])
parseImport = do
  try (top "import") 
  strictIndent 
  n <- parseModuleName
  o <- optional (between (res "(") (res ")") (many (notChar ')')))
  case o of
    Nothing -> return (Just n)
    Just _ -> return Nothing
    
              

parseModuleName :: Parser [String]
parseModuleName = lexeme (sepBy1 ((:) <$> upper <*> many alphaNum) (char '.'))

parseCon :: Parser a -> Parser (ConId, [a])
parseCon p = tuple <$> parseConId <*> many (strictIndent >> p)

tuple a b = (a,b)
      
parseTypeDef :: Parser (VarId, PType) 
parseTypeDef = sameIndent >> tuple <$> parseVarId <* res "::" <*> parseType

parseType :: Parser PType
parseType = foldr1 (:->) <$> sepBy1 (parseInnerType) (res "->")

parseInnerType :: Parser PType
parseInnerType = strictIndent >> (between (res "(") (res ")") parseType
               <|> Type <$> parseTypeId)



parseDefOp :: Parser (PExpr -> (VarId, PDef))
parseDefOp = do
  v <- parseInnerPattern 
  strictIndent
  o <- parseOpId 
  strictIndent
  v' <- parseInnerPattern 
  return (\e -> (o , PDef [v,v'] e))

parseDefVar :: Parser (PExpr -> (VarId, PDef))
parseDefVar = do
  v <- parseVarId
  ps <- many (strictIndent >> parsePattern)
  return (\e -> (v, PDef ps e))

parseDef :: Parser (VarId, PDef)
parseDef = do
  sameIndent
  d <- try parseDefOp <|> try parseDefVar 
  e <-  res "=" *> parseExpr
  return (d e)


parsePattern :: Parser Pattern
parsePattern = (PatVar <$> parseVarId)
            <|> (PatCon <$> parseConId <*> pure [])
            <|> between (reserved "(") (res ")") (strictIndent >> parseInnerPattern)

parseInnerPattern :: Parser Pattern
parseInnerPattern = (PatCon <$> parseConId <*> many (strictIndent >> parsePattern))
                 <|> parsePattern

parseExpr :: Parser PExpr
parseExpr = ($ id) <$> parseExpr'  

parseExpr' ::  Parser ((PExpr -> PExpr) -> PExpr)
parseExpr' = chainl1' (parseApp) parseOp 

-- parseCase <|>              

--parseCase :: Parser PExpr            
--parseCase = PCase <$> (try (res "case") *> parseExpr)
--                 <*> (res "of" *> block parseAlt) <*> return Nothing

chainl1' :: Alternative m => m a -> m (a -> a -> a) -> m ((a -> a) -> a)
chainl1' p op = scan where
  scan = (\a g f -> g (f a)) <$> p <*> rst 
  rst = (\f y g x -> g (f x y)) <$> op <*> p <*> rst <|> pure id

parseAlt :: Parser (PAlt PExpr)
parseAlt = PAlt <$> try parseInnerPattern 
               <*> (res "->" *> parseExpr)

parseApp :: Parser PExpr
parseApp = toApp <$> some parseInnerExpr

parseInnerExpr :: Parser PExpr
parseInnerExpr = strictIndent >> (    try (PVar <$> parseVarId) 
                                  <|> try (PCon <$> parseConId <*> pure [])
                                  <|> parseParens)

parseOp :: Parser (PExpr -> PExpr -> PExpr)
parseOp = flip POp <$> (strictIndent >> parseOpId)

parseParens :: Parser PExpr
parseParens = between (res "(") (res ")") $
              PParens . sortParens <$> (
                 (Left <$> do
                     e <- try parseApp
                     b <- optional $ do
                       o <- strictIndent >> parseOpId
                       es <- optional parseExpr' 
                       return (o,es)
                     return (e,b)
                 )
              <|> Right <$> do
                    o <- strictIndent >> parseOpId
                    e <- optional parseApp
                    return (o,e)
                    )
                     
--parseOverlap :: Parser VarId
--parseOverlap = do
--  string "{-#"
--  many (oneOf " \n")
--  string "OVERLAP"
--  x <- parseVarId <|> (char '(' *> parseOpId <* char ')' )
--  manyTill anyChar (try (string "#-}"))
----  justPragma
--  return x

--justPragma :: Parser ()
--justPragma = void $ manyTill anyChar (try string "{-#")
  
sortParens :: Either (PExpr, Maybe (OpId, Maybe ((PExpr -> PExpr) -> PExpr)))
                     (OpId, Maybe PExpr)
              -> PExpr
sortParens (Left (e, Nothing)) = e 
sortParens (Left (e, Just (o, Nothing))) = POpL e o
sortParens (Left (e, Just (o, Just f))) = f (POp e o)
sortParens (Right (o, Nothing)) = POpVar o
sortParens (Right (o, Just e)) = POpR o e

 

--parseExpr :: Parser (Expr, Bool)
--parseExpr =  (,) <$> parseCase <*> pure False
--         <|> parseOp <$> 

--parseOp :: Parser Expr                 
--parseOp = toOp <$> parseApp <*> optional (try $ do
--                                          strictIndent
--                                          o <- parseOpId
--                                          e <- parseExpr
--                                          return (o , e))
--
--          
--toOp :: Expr -> Maybe (String , Expr) -> Expr         
--toOp e = maybe e (\(s,e') -> Op e s e')  
--
--parseApp :: Parser Expr
--parseApp = toApp <$> some parseInnerExpr  --parseInnerExp
--
--parseInnerExpr :: Parser Expr
--parseInnerExpr = strictIndent >> (  try (Var <$> parseVarId)
--                               <|> try (ConE <$> parseConId <*> pure [])
--                               <|> parseParens )
--
--parseParens :: Parser Expr
--parseParens = Parens <$> between (res "(") (res ")") ( 
--     (do
--       e <- try parseExpr
--       case e of
--         (Case _ _) -> return e
--         (Op _ _ _) -> return e
--         _ -> do
--           r <- optional parseOpId
--           case r of
--             Just o -> return $ App (Var o) e
--             Nothing -> return e)
--     <|> Var <$> try parseOpId )
--
--parseAlt :: Parser Alt
--parseAlt = Alt <$> try (parseCon parseVarId)
--               <*> (res "->" *> parseExpr)
--
--
--parseCase :: Parser Expr
--parseCase = Case <$> (try (res "case") *> parseExpr)
--                 <*> (res "of" *> block parseAlt)
--
toApp :: [PExpr] -> PExpr
toApp = go . reverse
  where go [e] = e
        go (e : es) = PApp (go es) e


