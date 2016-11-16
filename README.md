# OverlapCheck

This is the proto-type implementation of a overlapping property based testing tool as described in the [Failing Faster paper](https://github.com/JonFowler/overlapCheck/blob/master/FailingFaster.pdf).

The performance supplement for the paper is the file [PerformanceSupplement.pdf](https://github.com/JonFowler/overlapCheck/blob/master/PerformanceSupplement.pdf)

## Installation:

The easiest way to install is using the [Haskell stack tool](https://docs.haskellstack.org/en/stable/README/#how-to-install) and then running *stack install* in the directory.

## Example use

There examples in the examples directory.

Example use:  
> overlapcheck Tree.hs  
> +++ Ok, successfully passed 100 tests in 2.50s

By default the tool will test the property named *check*. This can
be altered by using the *property* flag (*-p*).

The tool can also accept a *size* argument (*--sized*,*-s*). A natural
number which should be the first argument to the property.

> overlapcheck -p checkn -s5 Tree.hs  
> +++ Ok, successfully passed 100 tests in 2.35s


## Language

The proto-type implements a small functional language which is consistent
with a small subset of haskell. The language allows simple algebraic
data-types, pattern matching in definitions, type annotations for
definitions and module imports. It does not allow features such as
polymorphism and case expressions. GHC can be used to type-check 
and test any programs written in the subset.

### Overlapping pragma

A definition that uses of overlapping patterns should be accompanied
by a pragma:

> {-# OVERLAP (+) #-}  
> Z + x = x  
> x + Z = x  
> S x + y = S (x + y)  
> x + S y = S (x + y)  

### Distribution pragma

A algebraic data-type can be accompanied by a pragma giving the
frequency it should be chosen in needed narrowing (default it 1):

> data Tree = Node Tree Nat Tree | Leaf  
> {-# DIST Node 1 #-}  
> {-# DIST Leaf 5 #-}  
