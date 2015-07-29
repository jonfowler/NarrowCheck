{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Reach.Algebra.Lattice where

class Lattice a where 
  -- join, the lub
  join :: a -> a -> a
  -- meet, the glb
  meet :: a -> a -> a 
  

data a :+: b = InL a | InR b

data a :*: b = a :*: b

data K b a = K b

data One = One

data Lift a = Bot | Ins a

data family Lifted a 

