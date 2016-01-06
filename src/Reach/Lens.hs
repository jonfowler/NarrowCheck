module Reach.Lens 
  ( module Control.Lens,
    at'

  )where

import Control.Lens

at' :: (Show (Index m), At m) => Index m -> Lens' m (IxValue m)
at' l = at l . _unsafeJust (show l)

_unsafeJust :: String -> Lens (Maybe a) (Maybe b) a b 
_unsafeJust s f (Just a) = Just <$> f a
_unsafeJust s f Nothing = error $ "Failed to read the value: " ++ s ++ "of maybe in call to _unsafeJust"

