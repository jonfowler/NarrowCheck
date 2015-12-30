module Reach.Parser.ConReduce where

import Reach.Parser.Module

{- The following module evaluates the applications of a constructor
and then atomises the fields of the constructor-}


partial :: Expr -> Expr
partial (Case e as) = Case (partial e) (map partialAlt as)
    where partialAlt (Alt c e) = Alt c (partial e)
partial (App e e') = case partial e of
  ConE cid es -> ConE cid (es ++ [partial e'])
  ne -> App ne (partial e')
partial (Parens e) = Parens (partial e)
partial (ConE cid es) = ConE cid (map partial es)
partial (Var v) = Var v


