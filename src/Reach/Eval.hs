module Eval where

import Reach.Env
import Reach.Syntax
import Reach.Monad

import qualified Data.IntMap as I
import  Data.IntMap (IntMap)

bind :: Monad m => VarID -> Exp -> ReachT m ()
bind v e = modify (\s -> s { _env = I.insert (fromVarID v) e $ _env s}) 

inlineFun :: Monad m => FunID -> [Exp] -> ReachT m Exp 
inlineFun fid es = do
  s <- get
  let f = _funs s I.! fromFunID fid
  let nv = _nextVar s
  put (s {_nextVar = nv + varNum f})
  zipWithM_ bind (map (nv+) $ args f) es
  return (replaceVar nv $ body f)
 
