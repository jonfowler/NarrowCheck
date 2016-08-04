module Reach.Parser.Desugar where

import Reach.Parser.Module
import Reach.Parser.PExpr

import Control.Monad.State       
import Data.List
import Data.Function

import Reach.Lens


{- The following module evaluates the applications of a constructor
and then atomises the fields of the constructor-}

desugar :: PExpr -> PExpr
desugar =  partial . deOp



--desugar :: PExpr -> PExpr
--desugar = partial . deOp

deOp :: PExpr -> PExpr
deOp e = evalState (deOp' e) 0

deOp' :: PExpr -> State Int PExpr
--deOp' (PCase e as e') =
--  PCase <$> deOp' e <*> mapM deOp'Alt as <*> traverse deOp' e' 
--   where deOp'Alt (PAlt p e) = PAlt p <$> deOp' e
deOp' (PApp e e')  = PApp <$> deOp' e <*> deOp' e'
deOp' (PCon cid []) = return $ PCon cid []
deOp' (PVar v) = return $ PVar v
deOp' (PLam v e) = PLam v <$> deOp' e
deOp' (PLet v e e') = PLet v <$> deOp' e <*> deOp' e'

deOp' (PParens e) = deOp' e
deOp' (POp e o e') = PApp <$> (PApp (PVar o) <$> deOp' e) <*> deOp' e'
deOp' (POpVar o) = return $ PVar o
deOp' (POpL e o) = PApp (PVar o) <$> deOp' e
deOp' (POpR o e) = do  
  i <- get
  put (i+1)
  e' <- deOp' e
  let v = "##" ++ show v
  return . PLam v $ PApp (PApp (PVar o) (PVar v)) e'

data Bunch = Bunch ConId [([Pattern], PExpr)]
           | BunchVar VarId PExpr
      
partial :: PExpr -> PExpr
--partial (PCase e as e') = PCase (partial e) (map partialAlt as) (fmap partial e')
--    where partialAlt (PAlt c e) = PAlt c (partial e)
partial (PApp e e') = case partial e of
  PCon cid es -> PCon cid (es ++ [partial e'])
  ne -> PApp ne (partial e')
partial (PCon cid es) = PCon cid (map partial es)
partial (PVar v) = PVar v
partial (PLam v e) = PLam v (partial e)
partial (PLet v e e') = PLet v (partial e) (partial e')


newtype VarName = VarNum {_varNum :: Int}

newVarName :: State VarName VarId
newVarName = do
  VarNum i <- get
  put (VarNum $ i + 1)
  return ("#"++show i)
 

elemPatt :: VarId -> Pattern -> Bool
elemPatt v (PatVar v') = v == v' 
elemPatt v (PatCon _ ps) = any (elemPatt v) ps

subst :: VarId -> PExpr -> PExpr -> PExpr
-- subst v e (PCase e' as e'') = PCase (subst v e e')
--                                    (map substAlt as)
--                                    (fmap (subst v e) e'')
--    where substAlt (PAlt p e'') | v `elemPatt` p = PAlt p e
--                                | otherwise = PAlt p (subst v e e'')
subst v e (PApp e' e'') = PApp (subst v e e') (subst v e e'')
subst v e (PCon cid es) = PCon cid (map (subst v e) es) 
subst v e (PVar v') | v == v' = e
                    | otherwise = PVar v'
subst v e (PLam v' e') | v == v' = PLam v e'
                      | otherwise = PLam v (subst v e e')
subst v e (PLet v' e' e'') | v == v' = PLet v e' e''
                          | otherwise = PLet v (subst v e e') (subst v e e'')

isVar :: PDef -> Bool
isVar (PDef (PatVar v :_) _) = True
isVar _ = False

isCon :: PDef -> Bool
isCon (PDef (PatCon _ _ :_) _) = True
isCon _ = False

getCon :: PDef -> ConId
getCon (PDef (PatCon c _:_) _) = c

patCount :: [PDef] -> Int
patCount qs = length ((\(PDef (PatCon _ ps:_) _) -> ps) $ head qs)

--matcher :: [VarId] -> [PDef] -> Maybe PExpr -> State VarName (Maybe PExpr)
--matcher [] [] d = return d
--matcher [] (PDef [] e:_) _ = return $ Just e
--matcher [] _ _ = error "error in function matcher"
--matcher _ [] d = return d
--matcher us qs d | isVar (head qs) = do
--                    let (vs, qs') = span isVar qs
--                    d' <- matcher us qs' d
--                    matchVar us vs d'
--matcher us qs d | isCon (head qs) = do
--                    let (cs, qs') = span isCon qs
--                    d' <- matcher us qs' d
--                    matchCon us (sortBy (compare `on` getCon) cs) d'
--matcher us qs d = error $ "Incomplete pattern match in Desugar/matcher. The head of qs is: " ++ show (head qs)
--
--matchVar :: [VarId] -> [PDef] -> Maybe PExpr -> State VarName (Maybe PExpr)
--matchVar (u:us) qs = matcher us [PDef ps (subst v (PVar u) e) | PDef (PatVar v : ps) e <- qs]
--
--matchCon :: [VarId] -> [PDef] -> Maybe PExpr -> State VarName (Maybe PExpr)
--matchCon (u : us) qs d = do
--   as <- mapM (matchAlt us d ) (groupBy ((==) `on` getCon) qs)
--   return (Just $ PCase (PVar u) (as ++ defAlt) Nothing)
--     where defAlt = maybe [] ((:[]) . PAlt (PatVar "")) d 
--   
--matchAlt :: [VarId] -> Maybe PExpr -> [PDef] -> State VarName (PAlt PExpr)
--matchAlt us d qs = do
--  us' <- replicateM i newVarName
--  Just e' <- matcher (us' ++ us) [PDef (ps' ++ ps) e | PDef (PatCon c ps' : ps) e <- qs ] d
--  return (PAlt (PatCon c (map PatVar us')) e')
--
--  where c = getCon (head qs)
--        i = patCount qs
--
--depattern :: [PDef] -> PExpr
--depattern ps = flip evalState (VarNum 0) $ do
--  let i = length (_defArgs . head $ ps) 
--  us <- replicateM i newVarName
--  Just e <- matcher us ps Nothing
--  return (foldr (\u f -> PLam u . f) id us e)
  

--defPattern :: [Pattern] -> PExpr -> PExpr
--defPattern ps e = do
--  us <- replicateM (length ps) newVarName
--  match us ps e
--pattern :: [([(VarId, Pattern)], PExpr)] -> (VarId, [PAlt [([(VarId, Pattern)], PExpr)]])
--pattern = undefined

--  data PExpr
--  -- The following constructors are the "core" constructors, they are used for
--  -- parsing and then PExpr are converted to only use these types
--  = PCase PExpr [PAlt]
--  | PApp PExpr PExpr
--  | PCon ConId [PExpr]
--  | PVar VarId
--  | PLam VarId PExpr
--  | PLet VarId PExpr PExpr
--
--  -- The following are used in parsing but then desugared to the above constructors
--  | PParens PExpr
--  | POp PExpr OpId PExpr 
--  | POpVar OpId
--  | POpL PExpr OpId
--  | POpR OpId PExpr
--  deriving (Show)
 


--partial (POp e v e') = PApp (PApp (PVar v) (partial e)) (partial e')


