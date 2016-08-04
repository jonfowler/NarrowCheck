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
import Reach.Parser.PExpr

import Control.Arrow
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe
import Data.Foldable
import Control.Lens
import Control.Applicative

data Module = Module
  {_moduleName :: [String],
   _moduleImports :: [[String]],
   _moduleData :: Map TypeId [(ConId,[PType])],
   _moduleDef :: Map VarId ([PDef], Bool),  
   _moduleTypeDef :: Map VarId PType,
   _moduleCon :: Map ConId [PType]
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



addData :: PData -> StateT Module (Except String) ()
addData (tid, d) = do
  a <- use (moduleData . at tid)
  case a of
    Just _ -> throwError $ "Type "++ tid ++ " already defined\n"
    Nothing -> do moduleData . at tid ?= d
                  sequence_ (addCon <$> d)

addImport :: [String] -> StateT Module (Except String) ()
addImport i = moduleImports %= (i:)
--      return $ Module (M.insert tid d ds cs) defs tds
--
addDef :: (VarId, PDef) -> StateT Module (Except String) ()
addDef (vid, d) = 
  moduleDef . at vid %= Just . maybe ([d], False) (first (++ [d]))
--  a <- use (moduleDef . at vid)
--  case a of
--    Just _ -> throwError $ "Variable " ++ vid ++ " already defined\n"
--    Nothing -> 
-- where vid = d ^. defName

addTypeDef :: (VarId, PType) -> StateT Module (Except String) ()
addTypeDef (vid, d) = do
  a <- use (moduleTypeDef . at vid)
  case a of
    Just _ -> throwError $ "Type of variable " ++ vid ++ " already defined\n"
    Nothing -> moduleTypeDef . at vid ?= d

addCon :: (ConId, [PType]) -> StateT Module (Except String) ()
addCon (cid, ts) = do
  a <- use (moduleCon . at cid)
  case a of
    Just _ -> throwError $ "Constructor " ++ cid ++ " already defined\n"
    Nothing -> moduleCon . at cid ?= ts

--td@(TypeDef vid const) (Module ds defs tds) = case M.lookup vid tds of
--  Just _ -> throwError $ "Variable " ++ varId vid ++ "already has its type defined"
--  Nothing -> 
--
--    return $ Module ds defs (M.insert vid td tds) 

----addCons :: Cons -> Module -> Except String Module
----add
--
conToType :: (ConId, [PType]) -> TypeId -> PType
conToType (_, ts) tid = foldr (:->) (Type tid) ts

parserOfModule :: Parser (Except String Module)
parserOfModule = do
  nam <- whitespace >> parseModuleHead <|> return []
  imps <- catMaybes <$> many parseImport
  (execStateT <$> parseModule' <*> pure (emptyModule nam imps)) <* eof
   where parseModule' = sequence_ <$> many (
                   addData <$> try parseData
               <|> addDef <$> try parseDef
               <|> addTypeDef <$> parseTypeDef)

addOverlaps :: [Pragma] -> Module -> Module  
addOverlaps p m = foldr addOverlap m p

addOverlap :: Pragma -> Module -> Module  
addOverlap (Overlap p) m = moduleDef . ix p . _2 .~ True $ m

parseModule :: Monad m => String -> m Module
parseModule s = case runParse parserOfModule s of
  Failure err -> fail . show $ err
  Success (m, ps) -> case runExcept m of 
    Left err -> fail err
    Right m -> return (addOverlaps ps m)

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
checkTypeDefs m = sequence_ (checkTypeDef m <$> M.keys (m ^. moduleTypeDef))

checkTypeDef :: Module -> VarId -> Except String ()
checkTypeDef m f = case M.lookup f (m ^. moduleDef) of
  Nothing -> throwError $ "Type definition for " ++ f ++ " has no corresponding definiton\n"
  Just _ -> return () 

checkTypeScopes :: Module -> Except String ()
checkTypeScopes m = do
  mapMOf_ (moduleData . folded . folded . _2 . folded) (checkTypeScope m) m
  mapMOf_ (moduleTypeDef . folded) (checkTypeScope m) m

--checkTypeScopes :: Module -> Except String Module
--checkTypeScopes m = foldM checkTypeScope m (view  <$> m ^. moduleData)

checkTypeScope :: Module -> PType -> Except String ()
checkTypeScope m (s :-> t) =  checkTypeScope m s >> checkTypeScope m t
checkTypeScope m (Type tid) = case M.lookup tid (m ^. moduleData) of
  Just _ -> return () 
  Nothing -> throwError $ "Type " ++ tid ++ " not in scope\n"

checkScopes :: Module -> Except String ()
checkScopes m = mapMOf_ (moduleDef . folded . _1 . folded) (checkScopeDef m) m 

checkScopeDef :: Module -> PDef -> Except String ()
checkScopeDef m d = do
  locals <- foldM (checkPattern m) (M.empty) (d ^. defArgs)
  checkScopeExp m locals (d ^. defBody)

checkScopeExp :: Module -> Map VarId () -> PExpr -> Except String ()
--checkScopeExp m locals (PCase e as _) = checkScopeExp m locals e <||>
--   sequence_ (checkScopeAlt m locals <$> as)
checkScopeExp m locals (PApp e e') = checkScopeExp m locals e <||> checkScopeExp m locals e'
checkScopeExp m locals (PVar vid) = checkScope m locals vid
checkScopeExp m locals (PCon cid es) = mapM_ (checkScopeExp m locals) es 
checkScopeExp m locals (PLam v e) = checkScope m locals v
                                   <||> checkScopeExp m (M.insert v () locals) e

checkScopeExp m locals (PParens e) = checkScopeExp m locals e
checkScopeExp m locals (POp e v e') = checkScopeExp m locals e <||> checkScope m locals v <||>
                                     checkScopeExp m locals e'
checkScopeExp m locals (POpR v e') = checkScope m locals v <||> checkScopeExp m locals e'
checkScopeExp m locals (POpL e v) = checkScopeExp m locals e <||> checkScope m locals v

(<||>) :: MonadError e m => m a -> m a -> m ()
e <||> e' = do
  e
  e'
  return ()

checkScopeAlt :: Module -> Map VarId () -> PAlt PExpr -> Except String ()
checkScopeAlt m locals (PAlt p e) = do
  newlocals <- checkPattern m locals p 
  checkScopeExp m newlocals e 

checkPattern :: Module -> Map VarId () -> Pattern -> Except String (Map VarId ())
checkPattern m locals (PatVar v) = checkLocal m locals v
checkPattern m locals (PatCon cid ps) = foldM (checkPattern m) locals ps

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


