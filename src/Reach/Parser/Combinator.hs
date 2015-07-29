module Combinator where

open import Data.Maybe

data Parse a = Parse (String -> Maybe a)


