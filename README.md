# The Salty DSL

Salty is a domain specific language for describing `GR(1)` synthesis problems.

## Current State

Salty is currently under active development, and can be used to produce Java
source from a `GR(1)` specification.

## Language Goals

* Make controller specifications easier to read by
  - Allowing macro definitions to give a name to complex behavior
  - Adding syntactic-sugar for things like `if-then-else`
  - Allowing state variables to range over values of an enumeration, rather than
    just integer values
* Integrate with the slugs `GR(1)` synthesizer for the heavy lifting
* Generate controller implementations in python, and potentially java

## Building Salty

The easiest way to experiment with salty is to use the
[stack](http://docs.haskellstack.org/en/stable/README/#how-to-install) tool.
Once installed, stack will take care of installing the right version of the GHC
compiler, as well as any dependencies that salty has.

```shell
$ stack build
$ stack exec -- salty -h
```

The examples can be built by running `make` in the `examples/` directory, and
will place all of the generated Java code in the `examples/build` directory.
