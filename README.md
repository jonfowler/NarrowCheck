# overlapCheck

This is the proto-type implementation of a overlapping property based testing tool as described in the [Failing Faster paper](https://github.com/JonFowler/overlapCheck/blob/master/FailingFasterPaper.pdf).

## Installation:

The easiest way to install is using the [Haskell stack tool](https://docs.haskellstack.org/en/stable/README/#how-to-install) and then running *stack install* in the directory.

## Example use

There examples in the examples directory.

Example use:  
> overlapCheck Tree.hs  
> +++ Ok, successfully passed 100 tests in 2.501164s

By default the tool will test the property named *check*. This can
be altered by using the *property* flag, the property to be tested
must have an explicit type:
> overlapCheck -n 10 -p checkTrad Tree.hs  
> +++ Ok, successfully passed 10 tests in 8.431095s

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
