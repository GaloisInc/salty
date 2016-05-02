# The Salty DSL

Salty is a domain specific language for describing `GR(1)` synthesis problems.

## Current State

Salty is currently under active development, and is not functional at the
moment.

## Language Goals

* Make controller specifications easier to read by
  - Allowing macro definitions to give a name to complex behavior
  - Adding syntactic-sugar for things like `if-then-else`
  - Allowing state variables to range over values of an enumeration, rather than
    just integer values
* Integrate with the slugs `GR(1)` synthesizer for the heavy lifting
* Generate controller implementations in python, and potentially java
