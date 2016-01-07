module Prelude where

data Bool = True | False

not x = case x of
  True -> False
  False -> True

