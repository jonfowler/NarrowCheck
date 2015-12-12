module Reach.Lens 
  ( module Control.Lens,
    at'

  )where

import Control.Lens

at' :: At m => Index m -> Lens' m (IxValue m)
at' l = at l . _unsafeJust 

_unsafeJust :: Lens (Maybe a) (Maybe b) a b 
_unsafeJust f (Just a) = Just <$> f a
_unsafeJust f Nothing = error "Unsafe just failed to read value"    

