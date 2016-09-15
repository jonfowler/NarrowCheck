module Overlap.Parser.Conv where

import qualified Data.IntMap as I
import Data.IntMap (IntMap)
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe
import Data.List 
import Data.Function (on)

import Overlap.Lens
import Control.Monad.State
import Control.Monad.Except
import Control.Arrow

import Control.Applicative

import Overlap.Parser.Desugar
import Overlap.Parser.Module
import Overlap.Parser.PExpr

import Overlap.Eval.Expr       
import Overlap.Eval.Env

data Conv a = Conv
  { _mapToInt :: Map a Int
  , _mapFromInt ::  IntMap a
  , _nextInt :: Int
  }

makeLenses ''Conv

data Convert = Convert {
  _convertFuncId :: Conv VarId,
  _convertCon :: Conv ConId,
  _convertLocals :: Conv VarId,
  _convertTypes :: Conv TypeId,
  _conInfo :: Map ConId Int 
                       }

makeLenses ''Convert

type ConvertM = StateT Convert (Except String)  --ConvertM {runConvert :: (Convert -> (a , Int))}

viewConv :: Ord a => Getter Convert (Conv a) -> a -> ConvertM Int
viewConv f vid = do
  r <- use (f . mapToInt . at vid)
  case r of
    Just v -> return v
    Nothing -> throwError ""

viewCons :: ConId -> ConvertM Int
viewCons cid = viewConv convertCon cid
                     <|> throwError ("Constructor " ++ show cid ++ " does not exist")

convCon :: Convert -> VarId -> CId
convCon c cid = fromMaybe (error "Internal: convCon fail") $ M.lookup cid (c ^. convertCon . mapToInt)

setupConvert :: Module -> Convert 
setupConvert m = Convert
  { _convertFuncId = execState (setupConv (M.keys $ m ^. moduleDef)) emptyConv,
    _convertCon = execState (setupConv (M.keys $ m ^. moduleCon)) emptyConv,
    _convertTypes = execState (setupConv (M.keys $ m ^. moduleData)) emptyConv,
    _convertLocals = emptyConv,
    _conInfo = fmap length $ m ^. moduleCon 
  }

setupConv :: Ord a => [a] -> State (Conv a) ()
setupConv as = foldM_ (\_ v -> overConv id v) 0 as
                     
convModule :: Int -> Int -> Module -> Env Expr
convModule d i m = Env {
             _defs = I.fromList $ map (convDef c) (M.toList $ m ^. moduleDef),
             _defArgTypes = I.fromList $ map (convDefArg c) (M.toList $ m ^. moduleTypeDef),

             _free = I.empty,
             _nextFVar = 0,
             _freeDepth = I.empty,
             _freeType = I.empty,
             _freeCount = 0,

             _maxFreeCount = d,
             _maxDepth = i,
             _topFrees = [],

             _env = I.empty,
 --            _nextEVar = 0,
             _nextLVar = 0,

             _funcNames = c ^. convertFuncId . mapFromInt,
             _funcIds = c ^. convertFuncId . mapToInt,

             _typeConstr = I.fromList $ map (convData c) (M.toList $ m ^. moduleData),
             _constrNames = c ^. convertCon . mapFromInt,
             _constrIds = c ^. convertCon . mapToInt, 
             _typeNames = c ^. convertTypes . mapFromInt
             }
  where c = setupConvert m

convData :: Convert -> (TypeId, [(ConId, Int, [PType])]) -> (Type, [(CId, Int, [Type])])
convData c (tid, cs) = (c ^. convertTypes . mapToInt . at' tid, map convConType cs)
   where convConType (cid, n, ts) = (c ^. convertCon . mapToInt . at' cid, n,
                                  map (convType c . simpleType) ts)

convDefArg :: Convert -> (VarId, PType) -> (FId, [Type])
convDefArg c (vid, t) = (c ^. convertFuncId . mapToInt .at' vid, map (convType c) (getArgs t))

convType :: Convert -> TypeId -> Type
convType c tid = c ^. convertTypes . mapToInt . at' tid

simpleType :: PType -> TypeId
simpleType (Type tid) = tid 

getArgs :: PType -> [TypeId]
getArgs (Type _) = []
getArgs (Type tid :-> t) = tid : getArgs t

convDef :: Convert -> (VarId, ([PDef], Bool)) -> (FId, ([Int], Def))
convDef c (vid, (pds, b)) = ( fromMaybe (error "Function not found")
                                   $ c ^. convertFuncId . mapToInt . at vid,
                            ( lvs
                            , (if b then convOverlapDef else convOrderedDef) lvs cps ))
    where lvs = leadVars bckb 
          bckb = backbone $ map fst ps 
          ps = map ((map (convertCons c) . _defArgs) &&& _defBody)  pds
          cps = map (convPDef c bckb) ps 
                            

          
--  case runExcept . runStateT newd $ c of
--  Left e -> error e
--  Right (d, _) -> (
--     fromMaybe (error "Function not found") $ c ^. convertFuncId . mapToInt . at vid, 
--     d)
--  where newd | b = undefined -- convOverlapDef pds
--             | otherwise = convOrderedDef' pds
         

--maxPattern :: [Pattern] -> Int
--maxPattern = maximum . map varPattern

data BackBone a = Node a (IntMap [BackBone a])

bb (Node a _) = a

convertCons :: Convert -> Pattern' ConId a -> Pattern' CId a
convertCons c p' = mapPattern1 (convCon c) p'

mapPattern1 :: (a->b) -> Pattern' a c -> Pattern' b c
mapPattern1 f (PatVar a) = PatVar a 
mapPattern1 f (PatCon cid ps) = PatCon (f cid) (map (mapPattern1 f) ps)

patternName :: Pattern' CId VarId -> BackBone Int -> State Convert CPattern 
patternName (PatVar a) (Node i _) = do
   cid <- overConv convertLocals a
   convertLocals . nextInt += i - 1
   return $ PatVar cid 
patternName (PatCon cid ps) (Node i b) = do 
   cl <- use (convertLocals . nextInt)
   convertLocals . nextInt += 1 
   let bs = fromMaybe (error "Internal: backbone incorrect") $ I.lookup cid b
   ps' <- patternNames ps bs 
   convertLocals . nextInt .= cl + i 
   return (PatCon (cid, map (+(cl+1)) $ leadVars bs) ps')

patternNames :: [Pattern' CId VarId] -> [BackBone Int] -> State Convert [CPattern]
patternNames  = zipWithM patternName

convPDef :: Convert -> [BackBone Int] -> ([Pattern' CId VarId], PExpr) -> ([CPattern], Expr)
convPDef c b (ps, e) = (ps' , e') 
  where (ps' , c') = runState (patternNames ps b) $ c 
        e' = case runExcept . runStateT (convExpr . desugar $ e) $ c' of
               Left err -> error (err ++ " \n" ++ show ps ++ "\n" ++ show ps' ++ "\n" ++ show e ++
                                 "\n")
               Right (ne,_) -> ne


leadVars :: [BackBone Int] -> [LId]
leadVars [] = []
leadVars (Node i _ : bs) = 0 : map (+i) (leadVars bs)

backbone :: [[Pattern' Int a]] -> [BackBone Int]                
backbone = map (backboner . backboneBasic) . transpose
   where
     backboner :: BackBone () -> BackBone Int
     backboner (Node _ bs) = Node (maximum $ 1 : map ((1+) . sum . map bb) (I.elems bs')) bs'
        where bs' = I.map (map backboner) bs

     backboneBasic :: [Pattern' Int a] -> BackBone ()
     backboneBasic [] = Node () (I.empty)
     backboneBasic (p : ps) = intersectBB (backboneBasic' p) (backboneBasic ps)

     backboneBasic' :: Pattern' Int a -> BackBone ()
     backboneBasic' (PatVar _)  = Node () (I.empty)
     backboneBasic' (PatCon c ps) = Node () (I.singleton c (map backboneBasic' ps))
     
     intersectBB :: BackBone () -> BackBone () -> BackBone ()
     intersectBB (Node _ b) (Node _ b') = Node () (I.unionWith (zipWith intersectBB) b b')


varPattern :: Pattern -> Int
varPattern (PatVar a) = 1
varPattern (PatCon _ ps) = sum . map varPattern $ ps 

emptyPs :: [([Pattern' a b], c)] -> Bool
emptyPs (([],_) : _) = True
empryPs _ = False

type CPattern = Pattern' (CId, [Int]) LId

groupDefs :: [([CPattern], Expr)] -> ([(CId, [Int], [([CPattern], Expr)])],
                                      Maybe (LId, [([CPattern],Expr)]))
groupDefs [] = ([],Nothing)
groupDefs xs@(((PatCon (cid, is) _ : _), e) : _) = first ((cid, is, map getPEs ys):) $ groupDefs ys'
    where (ys, ys') = span (\(x:_,_) -> not (isPatVar x) && cid == fst (getPatCon x)) xs 
          getPEs (PatCon _ cps : cps', e)  = (cps ++ cps', e)
groupDefs ((PatVar vid : cps, e) : xs) = second 
                             (maybe (Just (vid,[(cps, e)])) $ \(vid,es) -> Just (vid, (cps,e):es))
                             $ groupDefs xs

convOverlapDef :: [Int] -> [([CPattern], Expr)] -> Def
convOverlapDef is ps = convOverlapDef' is $ sortBy (compare `on` fst) ps

convOverlapDef' :: [Int] -> [([CPattern], Expr)] -> Def
convOverlapDef' _ (([], e) : _) = Result (usedVars e) e
convOverlapDef' (_ : is) ps | all (isPatVar . head . fst) ps = convOverlapDef is (map (first tail) ps)
convOverlapDef' (i : is) ps = case groupDefs ps of
  (alts, ol) -> Match i (I.empty) (toAlt <$> alts) (I.empty) (convOverlapDef is . snd <$> ol)
     where toAlt (cid, is', ps') = Alt cid is' $ convOverlapDef (is' ++ is) ps'

--  where ps' = sortBy (compare `on` (head . fst)) ps

isPatVar :: Pattern' a b -> Bool
isPatVar (PatVar _) = True
isPatVar _ = False

convOrderedDef :: [Int] -> [([CPattern], Expr)] -> Def
convOrderedDef is ps = fromJust $ convOrderedDef' is ps Nothing

convOrderedDef' :: [Int] -> [([CPattern], Expr)] -> Maybe Def -> Maybe Def
convOrderedDef' _ [] d = d
convOrderedDef' _ (([], e) : _) _ = Just $ Result (usedVars e) e
convOrderedDef' is ps d | (isVar . head) ps = let
                                 (vs, ps') = span isVar ps
                                 d' = convOrderedDef' is ps' d
                               in convOrderedDef' (tail is) (first tail <$> vs) d'
   where isVar = isPatVar . head . fst  
convOrderedDef' is ps d | (isCon . head) ps = let
                                 (cs, ps') = span isCon ps
                                 d' = convOrderedDef' is ps' d
                               in matchCon is (sortBy (compare `on` getCon) cs) d'

   where isCon = not . isPatVar . head . fst 
         getCon = fst . getPatCon . head . fst

matchCon  :: [Int] -> [([CPattern], Expr)] -> Maybe Def -> Maybe Def
matchCon (i : is) ps d = Just $ Match i (I.empty) alts (I.empty) Nothing
   where alts = map (matchAlt is d) (groupBy ((==) `on` getCon) ps) ++ defAlt
         defAlt = maybe [] ((:[]) . AltDef) d
         getCon = fst . getPatCon . head . fst
matchCon is ps d = error $ show is ++ "\n" ++ show ps 

matchAlt :: [Int] ->  Maybe Def -> [([CPattern], Expr)] -> Alt 
matchAlt is d ps = Alt c vs d' 
  where  (c, vs) = getPatCon . head . fst . head $ ps
         Just d' = convOrderedDef' (vs ++ is) [((qs' ++ qs), e) | (PatCon c qs' : qs, e) <- ps] d
          
--convOrdVar :: [Int] -> [([CPattern], Expr)] -> Maybe Def -> Def
--convOrdVar (i : is) ps d = 


--convFunc :: Convert -> (VarId, [PDef]) -> (FId, Def)
--convFunc c (vid, qs) = case runExcept . runStateT s $ c of
--  Left e -> error e
--  Right (e , c') -> (
--    fromMaybe (error "Function not found") (c' ^. convertFuncId . mapToInt . at vid) ,
--    Func {_body = lazify e,
--          _vars = c' ^. convertLocals . nextInt
--          })
-- where s = convExpr (desugar qs) 

convArgs :: [VarId] -> ConvertM (Expr -> Expr)
convArgs [] = return id
convArgs (v : vs) = do
  i <- overConv convertLocals v 
  f <- convArgs vs
  return (Lam i . f)

atomise :: Expr -> ConvertM (Expr -> Expr, Atom)
atomise (Let x e e') = do
  (f , a') <- atomise e'
  return (Let x e . f , a')
atomise e | atom e = return (id, e)
          | otherwise = do
              x <- overConv convertLocals ""
              return (Let x e, Var x)

--(Expr e []) = return (id, e)
--atomise e = do
--  x <- overConv convertLocals  ""
--  return (Let x e , Var x)

atomises :: [Expr] -> ConvertM (Expr -> Expr, [Atom])
atomises es = foldr (\(f , a) (g , as) -> (f . g, a : as)) (id , []) <$> mapM atomise es

usedVars :: Expr -> [Int]
usedVars = I.keys . usedVars'

usedVars' :: Expr -> I.IntMap ()              
usedVars' (App e es) = I.union (usedVars' e) $ foldr (\e vs -> I.union (usedVars' e) vs) I.empty es
usedVars' (Let v e e') = I.insert v () $ I.union (usedVars' e) (usedVars' e')
usedVars' (Var v) = I.singleton v ()
usedVars' (Lam v e) = I.insert v () (usedVars' e)
usedVars' (Con c es) = foldr (\e vs -> I.union (usedVars' e) vs) I.empty es
usedVars' _ = I.empty
 
convExpr :: PExpr -> ConvertM Expr
convExpr (PVar vid) = (Var <$> viewConv convertLocals vid) 
                    <|> (Fun <$> viewConv convertFuncId vid) 
                    <|> throwError ("Variable " ++ show vid ++ " is not in local or function names")
convExpr (PCon cid es) = do
  es' <- mapM convExpr es
  (f, as) <- atomises es'
  c <- viewConv convertCon cid
  return . f $ (Con c as) 
convExpr (PApp e1 e2) = let (f : es) = joinApps e1 [e2]
                        in App <$> convExpr f <*> mapM convExpr es
  where joinApps (PApp e e') es = joinApps e (e' : es)
        joinApps e es = e : es
convExpr (PLam v e) = do
  v' <- overConv convertLocals v
  e' <- convExpr e
  return (Lam v' e')
  
convExpr (PParens e) = convExpr e
--convExpr (PCase e as) = Case <$> convExpr e <*> pure Bottom <*> mapM convAlt as


--convAlt :: PAlt PExpr -> ConvertM (Alt Expr)
--convAlt (PAlt (PatCon cid xs)  e) = do
--  cs <- viewCons cid
--  vs <- mapM (overConv convertLocals) (map aVar xs)
--  e' <- convExpr e
--  return (Alt cs vs e') 
--convAlt (PAlt (PatVar "") e) = AltDef <$> convExpr e 
--convAlt c = error ("convAlt incomplete patterns: " ++ show c)

overConv :: (MonadState s m, Ord a) => Simple Lens s (Conv a) -> a -> m Int
overConv l a = do
     i <- use (l . nextInt)
     l . mapToInt . at a ?= i
     l . mapFromInt . at i ?= a
     l . nextInt .= i + 1
     return i

updConv :: (MonadState s m, Ord a) => Simple Lens s (Conv a) -> a -> m Int
updConv l a = do
     r <- use (l . mapToInt . at a)
     case r of
       Nothing -> overConv l a 
       Just i -> return i

emptyConv :: Conv a
emptyConv = Conv
  { _mapToInt = M.empty,
    _mapFromInt = I.empty,
    _nextInt = 0
  }
