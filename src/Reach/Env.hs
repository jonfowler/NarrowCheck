module Reach.Env where

import Reach.Syntax
import qualified Data.IntMap as I
import  Data.IntMap (IntMap)


data Env = Env
  { env :: IntMap Exp
  , nextVar :: Int
  }
