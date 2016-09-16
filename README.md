# overlapCheck

This is the proto-type implementation of a overlapping property based testing tool as described in the [Failing Faster paper](https://github.com/JonFowler/overlapCheck/blob/master/FailingFasterPaper.pdf).

## Installation:

The easiest way to install is using the [Haskell stack tool](https://docs.haskellstack.org/en/stable/README/#how-to-install) and then running **stack install** in the directory.

## Example use

There examples in the examples directory.

Example use:  
> overlapCheck Tree.hs  
> +++ Ok, successfully passed 100 tests in 2.501164s

By default the tool will test the property named **check**. This can
be altered by using the **property** flag, the property to be tested
must have an explicit type:
> overlapCheck -n 10 -p checkTrad Tree.hs  
> +++ Ok, successfully passed 10 tests in 8.431095s

