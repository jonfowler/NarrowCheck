{-# LANGUAGE TemplateHaskell #-}
module Reach.Parser.Module where

import Reach.Parser.Tokens hiding ((<|>), many)
import Reach.Parser.Parse hiding ((<|>), many)
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as M
import Data.Map (Map)
import Data.Foldable
import Control.Lens
import Control.Applicative

data Module = Module
  {_moduleData :: Map TypeId Data,
   _moduleDef :: Map VarId Def,  
   _moduleTypeDef :: Map VarId TypeDef,
   _moduleCon :: Map ConId (Con Type)
  } deriving (Show)
makeLenses ''Module

emptyModule :: Module
emptyModule = Module
  { _moduleData = M.empty,
    _moduleDef = M.empty,
    _moduleTypeDef = M.empty,
    _moduleCon = M.empty
  }


addData :: Data -> StateT Module (Except String) ()
addData d = do
  a <- use (moduleData . at tid)
  case a of
    Just _ -> throwError $ "Type "++ typeId tid ++ " already defined\n"
    Nothing -> do moduleData . at tid ?= d
                  sequence_ (addCon <$> (d ^. dataCon))
 where tid = d ^. dataName

--      return $ Module (M.insert tid d ds cs) defs tds
--
addDef :: Def -> StateT Module (Except String) ()
addDef d = do
  a <- use (moduleDef . at vid)
  case a of
    Just _ -> throwError $ "Variable " ++ varId vid ++ " already defined\n"
    Nothing -> moduleDef . at vid ?= d
 where vid = d ^. defName

addTypeDef :: TypeDef -> StateT Module (Except String) ()
addTypeDef d = do
  a <- use (moduleTypeDef . at vid)
  case a of
    Just _ -> throwError $ "Type of variable " ++ varId vid ++ " already defined\n"
    Nothing -> moduleTypeDef . at vid ?= d
 where vid = d ^. typeDefName

addCon :: Con Type -> StateT Module (Except String) ()
addCon d = do
  a <- use (moduleCon . at cid)
  case a of
    Just _ -> throwError $ "Constructor " ++ conId cid ++ " already defined\n"
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

parseModule :: Parser (Except String Module)
parseModule = whitespace >> execStateT <$> parseModule' <*> pure emptyModule
   where parseModule' = sequence_ <$> many (
                   addData <$> parseData
               <|> addDef <$> parseDef
               <|> addTypeDef <$> parseTypeDef)

loadModule :: Parser (Either String Module)
loadModule = runExcept <$> do
  errorm <-parseModule
  return $ checks errorm
    where
      checks :: Except String Module -> Except String Module
      checks em = do
             m <- em 
             checkTypeDefs m
             checkScopes m
             return m


checkTypeDefs :: Module -> Except String ()
checkTypeDefs m = sequence_ (fmap (checkTypeDef m) (m ^. moduleTypeDef))

checkTypeDef :: Module -> TypeDef -> Except String ()
checkTypeDef m td = case M.lookup (td ^. typeDefName) (m ^. moduleDef) of
  Nothing -> throwError $ "Type definition for " ++ varId (td ^. typeDefName) ++ " has no corresponding definiton\n"
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
  Nothing -> throwError $ "Type " ++ typeId tid ++ " not in scope\n"

checkScopes :: Module -> Except String ()
checkScopes m = mapMOf_ (moduleDef . folded) (checkScopeDef m) m 

checkScopeDef :: Module -> Def -> Except String ()
checkScopeDef m d = mapM_ (checkLocal m M.empty) (d ^. defArgs) >>
                    checkScopeExp m M.empty (d ^. defBody)

checkScopeExp :: Module -> Map VarId () -> Exp -> Except String ()
checkScopeExp m locals (Case e as) = checkScopeExp m locals e <|>
   asum (checkScopeAlt m locals <$> as)
checkScopeExp m locals (App e e') = checkScopeExp m locals e <|> checkScopeExp m locals e'
checkScopeExp m locals (Parens e) = checkScopeExp m locals e
checkScopeExp m locals (Var vid) = checkScope m locals vid
checkScopeExp m locals (ConE cid) = return ()

checkScopeAlt :: Module -> Map VarId () -> Alt -> Except String ()
checkScopeAlt m locals a = do
  newlocals <- foldlMOf (altPattern . conArgs . folded)  (checkLocal m) locals a
  checkScopeExp m newlocals (a ^. altBody)


checkScope :: Module -> Map VarId () -> VarId ->  Except String ()
checkScope m locals vid = case M.lookup vid (m ^. moduleDef) of
  Just _ -> return ()
  Nothing -> case M.lookup vid locals of
    Just _ -> return ()
    Nothing -> throwError $ "Variable " ++ varId vid ++ " not in scope\n"


checkLocal :: Module -> Map VarId () -> VarId -> Except String (Map VarId ()) 
checkLocal m locals vid = case M.lookup vid (m ^. moduleDef) of
  Just _ -> throwError $ "Local variable " ++ varId vid ++ " already in use\n"
  Nothing -> case M.lookup vid locals of
    Just _ -> throwError $ "Local variable " ++ varId vid ++ " already in use\n"
    Nothing -> return (M.insert vid () locals) 


