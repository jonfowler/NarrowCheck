module Reach.Printer
  (printExpr,
--   printState,
   printDoc,
   printFuncs,
   putDoc
   )where

import Prelude hiding ((<$>))
import Text.PrettyPrint.Leijen

import Reach.Lens
import Reach.Eval.Expr
import Reach.Eval.ExprBase
import qualified Reach.Eval.Env as E

import qualified Data.IntMap as I

--printExpr :: Env -> Expr -> Doc
--printExpr s e = (bracketer s . reverse . appList $ e)

bracketer :: [(Doc,Bool)] -> Doc
bracketer [(d , _)] = d 
bracketer ds = group . nest 2 . vsep $ map bracket ds

bracket :: (Doc, Bool) -> Doc
bracket (d , True) = text "(" <> d <> text ")"
bracket (d , False) = d

var :: String -> Int -> Doc
var s i = text (s ++ show i)

printExpr :: E.Env Expr -> Expr -> Doc
printExpr s = fst . printExpr' s

printExpr' :: E.Env Expr -> Expr -> (Doc, Bool)
printExpr' s (Let v e e') = (text "let" <+> var "v" v
                                        <+> text "="
                                        <+> printExpr s e
                                        <+> text "in"
                                        <+> printExpr s e'
                            , True)
printExpr' s (Case e e' as) = (text "case"
                                      <+> nest 2 (printExpr s e
                                      <+>  (text "of" <$>
                                              (vsep (text ">>>" <+> printExpr s e' : map (printAlt s (printExpr s)) as))))
                           , True) 
printExpr' s e@(App _ _) = (bracketer . map (printExpr' s) . reverse . allApps $ e, True)

printExpr' s (Lam x e) = (text "\955" <+> var "v" x <+> text "->" <+> printExpr s e , True)
printExpr' s (Var x) = (var "v" x , False)
printExpr' s (FVar x) = (var "x" x , False)
printExpr' s (Con cid []) = (text (s ^. E.constrNames . at' cid)
                            , False)
printExpr' s (Con cid es) = (text (s ^. E.constrNames . at' cid)
                            <+> (group . hsep . map (bracket . printExpr' s) $ es)
                            , True)
printExpr' s (Fun fid) = (text (s ^. E.funcNames . at' fid), False)
printExpr' s Bottom = (text "BOT", False)

allApps :: Expr -> [Expr]
allApps (App e e') = e' : allApps e 
allApps e = [e]
 
--printExpr :: Env -> Expr -> Doc
--printExpr s =  fst . printExpr' s 

--toAppList :: Expr -> [Expr]
--toAppList (Expr e (Apply e' : cs)) = e : toAppList (Expr e' cs)
--toAppList e = [e]

--printExpr' :: Env -> Expr -> (Doc, Bool)
--printExpr' s (Let v e e') = (text "let" <+> var "v" v
--                                        <+> text "="
--                                        <+> printExpr s e
--                                        <+> text "in"
--                                        <+> printExpr s e'
--                            , True)
--printExpr' s (Expr e cs) = printConts s (printExpr' s e) cs

--printConts :: Env -> (Doc , Bool) -> [Conts] -> (Doc, Bool)
--printConts s d [] = d
--printConts s (d , _) (Branch _ as : cs) = printConts s (text "case"
--                                      <+> nest 2 (d
--                                      <+>  (text "of" <$>
--                                              (vsep . map (printAlt s (printExpr s))) as))
--                           , True) cs
--
--printConts s d cs@(Apply _ : _) =  let (es , cs') = allApps cs in
--  printConts s (bracketer (d : map (printExpr' s) es), True) cs'
--
--allApps :: [Conts] -> ([Expr] , [Conts])
--allApps (Apply e : cs) = let (es , cs') = allApps cs in (e : es, cs')
--allApps cs = ([], cs)
                                                        


  --(text $ s ^. funcNames . at' fid, False)
--printExpr' s e@(App _ _) = (printExpr s e, True)
--printExpr' s (Case e as) = (text "case" <+>
--                            printExpr s e <+> 
--                            nest 2 (text "of" <$>
--                                (vsep . map (printAlt s (printExpr s))) as)
--                           , True)
-- 
printAlt :: E.Env Expr -> (a -> Doc) -> Alt a -> Doc
printAlt s p (Alt cid [] e) = text (s ^. E.constrNames . at' cid)
  <+> text "->"
  <+> p e
printAlt s p (Alt cid vs e) = text (s ^. E.constrNames . at' cid)
  <+> hsep (map (var "v") vs)
  <+> text "->"
  <+> p e
printAlt s p (AltDef e) = text "_" 
  <+> text "->"
  <+> p e

--printCont :: Env -> Cont -> Doc
--printCont s e = printExpr s (toExpr e) 
--

printFuncs :: E.Env Expr -> Doc
printFuncs s = vsep . map printEnv' $ I.toList (s ^. E.funcs)
 where printEnv' (x , f) = text (s ^. E.funcNames . at' x) <+> text "=>" <+> printExpr s (f ^. body) 
  
--printEnv :: Env -> Doc
--printEnv s = vsep . map printEnv' $ I.toList (s ^. env)
--   where printEnv' (x , e) = var "e" x <+> text "=>" <+> printExpr s e

--printState :: Expr -> Env -> Doc
--printState e s = text "EXPR:"
--              <$> printExpr s e
--              <$> text "ENV:"
--              <$> printEnv s
--              <> line <> line

printDoc :: Doc -> String
printDoc = printSimpleDoc . renderPretty 1 90

printSimpleDoc :: SimpleDoc -> String
printSimpleDoc SEmpty = ""
printSimpleDoc (SChar c sd) = c : printSimpleDoc sd
printSimpleDoc (SText _ s sd) = s ++ printSimpleDoc sd
printSimpleDoc (SLine i sd) = '\n' : replicate i ' ' ++ printSimpleDoc sd

--appList :: Expr -> [Expr]
--appList (App e e') = e' : appList e 
--appList e = [e]

