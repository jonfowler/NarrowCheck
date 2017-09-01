module Huffman where

import Prelude()
import OverlapPrelude

data Encoding = Cb Bool Encoding | Em
data String = Cr Char String | Emp
data Freq = C Char Nat Freq | E
data Tree = Leaf Char | Fork Tree Tree
data Char = U | V | W | X | Y | Z

U =|= U = True
V =|= V = True
W =|= W = True
X =|= X = True
Y =|= Y = True
x =|= y = False

decode :: Tree -> Encoding -> String
decode t Em =  Emp
decode t bs = dec t t bs

dec :: Tree -> Tree -> Encoding -> String
dec (Leaf x) t bs = Cr x (decode t bs)
dec (Fork xt yt) t (Cb False bs) = dec xt t bs
dec (Fork xt yt) t (Cb True bs) = dec yt t bs

encode :: Tree -> String -> Encoding
encode t cs = enc (codetable t) cs

enc :: Tree -> String -> Encoding
enc table Emp = Em
enc table (Cr c cs) = (lookup c table) ++ enc table cs

lookup :: Char -> Tree -> Encoding
lookup = undefined

--collate (c:cs) = insert (S n, Leaf c) (collate ds)
--  where (n, ds) = count c cs
--
--count x [] = (Z, [])
--count x (y:ys) = if x *=* y then (S n, zs) else (n, y:zs)
--  where (n, zs) = count x ys
--
--insert (w, x) [] = [(w, x)]
--insert (w0, x) ((w1, y):wys)
--  | w0 *<=* w1 = (w0, x) : (w1, y) : wys
--  | otherwise = (w1, y) : insert (w0, x) wys
--
--hufftree cs = mkHuff (collate cs)
--
--mkHuff [(w, t)] = t
--mkHuff ((w0, t0):(w1, t1):wts) =
--  mkHuff (insert (w0*+*w1, Fork t0 t1) wts)
--
--((x, bs) : xbs) ! y = if x *=* y then bs else xbs ! y
--
--codetable t = tab [] t
--
--tab p (Leaf x) = [(x,p)]
--tab p (Fork xt yt) = tab (p++[False]) xt ++ tab (p++[True]) yt
--
--collate [] = []

