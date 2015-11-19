{-# LANGUAGE TemplateHaskell #-}

module Reach.Eval.Env where

import Reach.Eval.Expr

import Data.IntMap
import Control.Lens

data Env = Env {
  _funcs :: IntMap Func,
  _frees :: IntMap (CId, [CId]),
  _env :: IntMap (
  _nextVar :: LId
  }

makeLenses ''Env


