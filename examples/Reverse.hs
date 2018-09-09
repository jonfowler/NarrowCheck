module Reverse where

import Prelude ()
import Huffman
import OverlapPrelude

{-# DIST E 1 #-}
{-# DIST C 5 #-}

checkBasic :: List Char -> List Char -> Result
checkBasic l l' = post (eqListTrad (=|=) (reverse (l ++ l')) (reverse l' ++ reverse l))

