{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Reach.Eval.Exp where

newtype LVarId = LVarId Int deriving (Eq, Show, Read, Num)
newtype GVarId = GVarId Int deriving (Eq, Show, Read, Num)

newtype ConId = ConId Int deriving (Eq, Show, Read, Num) 
newtype FunId = FunId Int deriving (Eq, Show, Read, Num)

data Fun = Fun {_funId :: FunId, _funBody :: Exp} 

data Exp = ELet LVarId Exp Exp
         | ECase Exp [Alt]
         | EApp Exp Atom
         | ECon ConId [Atom]
         | ELVar LVarId
         | EGVar GVarId
         | EFun FunId

data Alt = Alt ConId LVarId Exp

type Atom = Exp 

