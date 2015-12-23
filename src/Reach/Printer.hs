module Reach.Printer
  (printExpr,
   putDoc
   )where

import Prelude hiding ((<$>))
import Text.PrettyPrint.Leijen

import Reach.Lens
import Reach.Eval.Expr
import Reach.Eval.Env

printExpr :: Env -> Expr -> Doc
printExpr s = bracketer s . reverse . appList 

bracketer :: Env -> [Expr] -> Doc
bracketer s [e] = fst $ printExpr' s e
bracketer s es = group . vsep $ map (bracketExpr s) es

bracketExpr :: Env -> Expr -> Doc
bracketExpr s e = case printExpr' s e of
  (d , True) -> text "(" <> d <> text ")"
  (d , False) -> d

var :: String -> Int -> Doc
var s i = text (s ++ show i)

printExpr' :: Env -> Expr -> (Doc, Bool)
printExpr' s (Let v e e') = (text "let" <+> var "v" v
                                        <+> text "="
                                        <+> printExpr s e
                                        <+> text "in"
                                        <+> printExpr s e'
                            , True)
printExpr' s (Fun fid) = (text $ s ^. funcNames . at' fid, False)
printExpr' s (EVar x) = (var "e" x , False)
printExpr' s (LVar x) = (var "v" x , False)
printExpr' s (FVar x) = (var "x" x , False)
printExpr' s e@(App _ _) = (printExpr s e, True)
printExpr' s (Lam x e) = (text "\955" <+> var "v" x <+> text "->" <+> printExpr s e , True)
printExpr' s (Case e as) = (text "case" <+>
                            printExpr s e <+> 
                            nest 2 (text "of" <$>
                      (vsep . map (printAlt s (printExpr s))) as)
                           , True)
printExpr' s (Con cid es) = (text (s ^. constrNames . at' cid)
                            <+> (group . hsep . map (bracketExpr s) $ es)
                            , True)
  
printAlt :: Env -> (a -> Doc) -> Alt a -> Doc
printAlt s p (Alt cid vs e) = text (s ^. constrNames . at' cid)
  <+> hsep (map (var "v") vs)
  <+> text "->"
  <+> p e
{- 
data Expr
  = Let !LId Expr Expr
  | Fun {-# UNPACK #-} !FuncId
  | EVar !EId
  | LVar !LId
  | FVar !FId
  | App Expr Expr 
  | Lam !LId Expr
  | Case Expr [Alt Expr] 
  | Con !CId [Atom] deriving Show
-}

appList :: Expr -> [Expr]
appList (App e e') = e' : appList e 
appList e = [e]

