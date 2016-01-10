module Reach.Parser.Module (
  module Reach.Parser.Tokens,
  module Reach.Parser.Parse,

  Module,
  moduleData,
  moduleDef,
  moduleTypeDef,
  moduleCon,
  moduleImports,
  moduleName,

  parseModule,
  checkModule,
  mergeModules
  )where

import Text.Trifecta.Result
import Text.Parser.Combinators
import Reach.Parser.Tokens 
import Reach.Parser.Parse 
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as M
import Data.Map (Map)
import Data.Foldable
import Control.Lens
import Control.Applicative

data Module = Module
  {_moduleName :: [String],
   _moduleImports :: [[String]],
   _moduleData :: Map TypeId Data,
   _moduleDef :: Map VarId Def,  
   _moduleTypeDef :: Map VarId TypeDef,
   _moduleCon :: Map ConId (Con Type)
  } deriving (Show)
makeLenses ''Module

emptyModule :: [String] -> [[String]] -> Module
emptyModule nam imps = Module
  { _moduleName = nam,
    _moduleImports = imps,
    _moduleData = M.empty,
    _moduleDef = M.empty,
    _moduleTypeDef = M.empty,
    _moduleCon = M.empty
  }

mergeModules :: Monad m => Module -> [Module] -> m Module
mergeModules m [] = return m
mergeModules m (n : ns) = (mergeModules m ns) >>= \m' -> mergeModule' m'  n

mergeModule' :: Monad m => Module -> Module -> m Module
mergeModule' m n | null dataIntersect && null defIntersect
                   && null typedefIntersect && null conIntersect
                   = return $ m {_moduleData = M.union (m ^. moduleData) (n ^. moduleData),
                        _moduleDef = M.union (m ^. moduleDef) (n ^. moduleDef),
                        _moduleTypeDef = M.union (m ^. moduleTypeDef) (n ^. moduleTypeDef),
                        _moduleCon = M.union (m ^. moduleCon) (n ^. moduleCon)
                       }
                 | otherwise = fail "error overlapping namespaces"
                     

  where
    dataIntersect = M.intersection (m ^. moduleData) (n ^. moduleData)
    defIntersect = M.intersection (m ^. moduleData) (n ^. moduleData)
    typedefIntersect = M.intersection (m ^. moduleData) (n ^. moduleData)
    conIntersect = M.intersection (m ^. moduleData) (n ^. moduleData)



addData :: Data -> StateT Module (Except String) ()
addData d = do
  a <- use (moduleData . at tid)
  case a of
    Just _ -> throwError $ "Type "++ tid ++ " already defined\n"
    Nothing -> do moduleData . at tid ?= d
                  sequence_ (addCon <$> (d ^. dataCon))
 where tid = d ^. dataName

addImport :: [String] -> StateT Module (Except String) ()
addImport i = moduleImports %= (i:)
--      return $ Module (M.insert tid d ds cs) defs tds
--
addDef :: Def -> StateT Module (Except String) ()
addDef d = do
  a <- use (moduleDef . at vid)
  case a of
    Just _ -> throwError $ "Variable " ++ vid ++ " already defined\n"
    Nothing -> moduleDef . at vid ?= d
 where vid = d ^. defName

addTypeDef :: TypeDef -> StateT Module (Except String) ()
addTypeDef d = do
  a <- use (moduleTypeDef . at vid)
  case a of
    Just _ -> throwError $ "Type of variable " ++ vid ++ " already defined\n"
    Nothing -> moduleTypeDef . at vid ?= d
 where vid = d ^. typeDefName

addCon :: Con Type -> StateT Module (Except String) ()
addCon d = do
  a <- use (moduleCon . at cid)
  case a of
    Just _ -> throwError $ "Constructor " ++ cid ++ " already defined\n"
    Nothing -> moduleCon . at cid ?= d
 where cid = d ^. conName

--td@(TypeDef vid const) (Module ds defs tds) = case M.lookup vid tds of
--  Just _ -> throwError $ "Variable " ++ varId vid ++ "already has its type defined"
--  Nothing -> 
--
--    return $ Module ds defs (M.insert vid td tds) 

----addCons :: Cons -> Module -> Except String Module
----add
--
conToType :: Con Type -> TypeId -> Type
conToType (Con _ ts) tid = foldr (:->) (Type tid) ts

parserOfModule :: Parser (Except String Module)
parserOfModule = do
  nam <- whitespace >> parseModuleHead <|> return []
  imps <- many parseImport
  (execStateT <$> parseModule' <*> pure (emptyModule nam imps)) <* eof
   where parseModule' = sequence_ <$> many (
                   addData <$> try parseData
               <|> addDef <$> try parseDef
               <|> addTypeDef <$> parseTypeDef)

parseModule :: Monad m => String -> m Module
parseModule s = case runParse parserOfModule s of
  Failure err -> fail . show $ err
  Success m -> case runExcept m of 
    Left err -> fail err
    Right m -> return m

checkModule :: Monad m => Module -> m ()
checkModule m = case runExcept (checkTypeDefs m >> checkScopes m) of
  Left err -> fail err
  Right _ -> return ()
  
         
--loadModule :: Parser (Either String Module)
--loadModule = runExcept <$> do
--  errorm <-parseModule
--  return $ checks errorm
--    where
--      checks :: Except String Module -> Except String Module
--      checks em = do
--             m <- em 
--             checkTypeDefs m
--             checkScopes m
--             return m


checkTypeDefs :: Module -> Except String ()
checkTypeDefs m = sequence_ (fmap (checkTypeDef m) (m ^. moduleTypeDef))

checkTypeDef :: Module -> TypeDef -> Except String ()
checkTypeDef m td = case M.lookup (td ^. typeDefName) (m ^. moduleDef) of
  Nothing -> throwError $ "Type definition for " ++ td ^. typeDefName ++ " has no corresponding definiton\n"
  Just _ -> return () 

checkTypeScopes :: Module -> Except String ()
checkTypeScopes m = do
  mapMOf_ (moduleData . folded . dataCon . folded . conArgs . folded) (checkTypeScope m) m
  mapMOf_ (moduleTypeDef . folded . typeDefType) (checkTypeScope m) m

--checkTypeScopes :: Module -> Except String Module
--checkTypeScopes m = foldM checkTypeScope m (view  <$> m ^. moduleData)

checkTypeScope :: Module -> Type -> Except String ()
checkTypeScope m (s :-> t) =  checkTypeScope m s >> checkTypeScope m t
checkTypeScope m (Type tid) = case M.lookup tid (m ^. moduleData) of
  Just _ -> return () 
  Nothing -> throwError $ "Type " ++ tid ++ " not in scope\n"

checkScopes :: Module -> Except String ()
checkScopes m = mapMOf_ (moduleDef . folded) (checkScopeDef m) m 

checkScopeDef :: Module -> Def -> Except String ()
checkScopeDef m d = mapM_ (checkLocal m M.empty) (d ^. defArgs) >>
                    checkScopeExp m (M.fromList (map (\a -> (a,())) $ d ^. defArgs)) (d ^. defBody)

checkScopeExp :: Module -> Map VarId () -> Expr -> Except String ()
checkScopeExp m locals (Case e as) = checkScopeExp m locals e <||>
   sequence_ (checkScopeAlt m locals <$> as)
checkScopeExp m locals (App e e') = checkScopeExp m locals e <||> checkScopeExp m locals e'
checkScopeExp m locals (Parens e) = checkScopeExp m locals e
checkScopeExp m locals (Var vid) = checkScope m locals vid
checkScopeExp m locals (Op e v e') = checkScopeExp m locals e <||> checkScope m locals v <||>
                                     checkScopeExp m locals e'
checkScopeExp m locals (ConE cid es) = mapM_ (checkScopeExp m locals) es 

(<||>) :: MonadError e m => m a -> m a -> m ()
e <||> e' = do
  e
  e'
  return ()

checkScopeAlt :: Module -> Map VarId () -> Alt -> Except String ()
checkScopeAlt m locals a = do
  newlocals <- foldlMOf (altPattern . conArgs . folded)  (checkLocal m) locals a
  checkScopeExp m newlocals (a ^. altBody)


checkScope :: Module -> Map VarId () -> VarId ->  Except String ()
checkScope m locals vid = case M.lookup vid (m ^. moduleDef) of
  Just _ -> return ()
  Nothing -> case M.lookup vid locals of
    Just _ -> return ()
    Nothing -> throwError $ "Variable " ++ vid ++ " not in scope\n"


checkLocal :: Module -> Map VarId () -> VarId -> Except String (Map VarId ()) 
checkLocal m locals vid = case M.lookup vid (m ^. moduleDef) of
  Just _ -> throwError $ "Local variable " ++ vid ++ " already in use\n"
  Nothing -> case M.lookup vid locals of
    Just _ -> throwError $ "Local variable " ++ vid ++ " already in use\n"
    Nothing -> return (M.insert vid () locals) 


