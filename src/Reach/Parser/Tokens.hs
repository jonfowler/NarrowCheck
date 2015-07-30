module Tokens where

data Token =
  | TSpace
  | TLine
  | TWord String
  | TOp
  | TEq 
  | TCase
  | TOf
  | TData
  | TOp String
  | TWhere
  | TLet
  | TIn


