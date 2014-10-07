module Reach.Env where

import Reach.Syntax
import qualified Data.IntMap as I
import  Data.IntMap (IntMap)

type Defs = IntMap Def 

data Def = Def 
  { name :: String
  , varID :: Int
  , exp :: Exp
  }

data Env = Env
  { env :: IntMap Exp
  , nextVar :: Int
  }
