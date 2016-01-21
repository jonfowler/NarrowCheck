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
desugar = partial . deOp

deOp :: PExpr -> PExpr
deOp e = evalState (deOp' e) 0

deOp' :: PExpr -> State Int PExpr
deOp' (PCase e as) = PCase <$> deOp' e <*> mapM deOp'Alt as
   where deOp'Alt (PAlt p e) = PAlt p <$> deOp' e
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
partial (PCase e as) = PCase (partial e) (map partialAlt as)
    where partialAlt (PAlt c e) = PAlt c (partial e)
partial (PApp e e') = case partial e of
  PCon cid es -> PCon cid (es ++ [partial e'])
  ne -> PApp ne (partial e')
partial (PCon cid es) = PCon cid (map partial es)
partial (PVar v) = PVar v
partial (PLam v e) = PLam v (partial e)
partial (PLet v e e') = PLet v (partial e) (partial e')


newtype VarName = VarNum {_varNum :: Int}
makeLenses ''VarName

newVarName :: State VarName VarId
newVarName = do
  i <- use varNum
  varNum += 1
  return ("#"++show i)
 

elemPatt :: VarId -> Pattern -> Bool
elemPatt v (PatVar v') = v == v' 
elemPatt v (PatCon _ ps) = any (elemPatt v) ps

subst :: VarId -> PExpr -> PExpr -> PExpr
subst v e (PCase e' as) = PCase (subst v e e') (map substAlt as)
    where substAlt (PAlt p e'') | v `elemPatt` p = PAlt p e
                                | otherwise = PAlt p (subst v e e'')
subst v e (PApp e' e'') = PApp (subst v e e') (subst v e e'')
subst v e (PCon cid es) = PCon cid (map (subst v e) es) 
subst v e (PVar v') | v == v' = e
                    | otherwise = PVar v'
subst v e (PLam v' e') | v == v' = PLam v e'
                      | otherwise = PLam v (subst v e e')
subst v e (PLet v' e' e'') | v == v' = PLet v e' e''
                          | otherwise = PLet v (subst v e e') (subst v e e'')

type Equat = ([Pattern], PExpr)

isVar :: Equat -> Bool
isVar ((PatVar v :_),_) = True
isVar _ = False

isCon :: Equat -> Bool
isCon ((PatCon _ _ :_),_) = True
isCon _ = False

getCon :: Equat -> ConId
getCon ((PatCon c _:_),_) = c

matcher :: [VarId] -> [Equat] -> PExpr -> State VarName PExpr
matcher [] [] d = return d
matcher [] (([], e):_) _ = return e
matcher [] _ _ = error "error in function matcher"
matcher us qs d | isVar (head qs) = do
                    let (vs, qs') = span isVar qs
                    d' <- matcher us qs' d
                    matchVar us vs d'
matcher us qs d | isCon (head qs) = do
                    let (cs, qs') = span isCon qs
                    d' <- matcher us qs' d
                    matchCon us (sortBy (compare `on` getCon) cs) d'

matchVar :: [VarId] -> [Equat] -> PExpr -> State VarName PExpr
matchVar (u:us) qs = matcher us [(ps, subst v (PVar u) e) | (PatVar v : ps, e) <- qs]

matchCon :: [VarId] -> [Equat] -> PExpr -> State VarName PExpr
matchCon (u : us) qs d = do
   as <- mapM (matchAlt us d ) (groupBy ((==) `on` getCon) qs)
   return (PCase (PVar u) (as ++ [defAlt]))
     where defAlt = PAlt (PatVar "") d 
   
matchAlt :: [VarId] -> PExpr -> [Equat] -> State VarName (PAlt PExpr)
matchAlt us d qs = do
  us' <- replicateM i newVarName
  e' <- matcher (us' ++ us) [(ps' ++ ps, e) | (PatCon c ps' : ps, e) <- qs ] d
  return (PAlt (PatCon c (map PatVar us')) e')

  where c = getCon (head qs)
        i = length ((\((PatCon _ ps:_,_)) -> ps) $ head qs)


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


