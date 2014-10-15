module Reach.Eval
 (module X,
 bind,  
 look,  
 findAlt,
 inlineFun
 ) where

import Reach.Env as X
import Reach.Syntax as X
import Reach.Monad as X
import Data.List

import qualified Data.IntMap as I
import  Data.IntMap (IntMap)


import Data.Generics.Uniplate.Data

bind :: Monad m => VarID -> Exp -> ReachT m ()
bind v e = modify (insertV v e) 

look :: Monad m => VarID -> ReachT m (Maybe Exp)
look (VarID v) = do 
  m <- get    
  return $ I.lookup v $ _env m

inlineFun :: Monad m => FunID -> [Exp] -> ReachT m Exp 
inlineFun fid es = do
  s <- get
  let f = findF fid s 
  let nv = _nextVar s
  put (s {_nextVar = nv + varNum f})
  zipWithM_ bind (map (nv+) $ args f) es
  return (replaceVar nv $ body f)

findAlt :: ConID -> [Alt] -> Maybe Alt 
findAlt cid = find (\(Alt c _ _) -> c == cid)
 
replaceVar :: VarID -> Exp -> Exp
replaceVar i = transformBi f
  where f (VarID v) = VarID v + i
        f a = a
