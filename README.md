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

### Dependencies

It's easiest to build salty using the
[stack](http://docs.haskellstack.org/en/stable/README/#how-to-install) tool.
Stack will manage the installation of all haskell dependencies, as well as the
GHC compiler itself.

Additionally, you will need to build and install the
[slugs](https://github.com/VerifiableRobotics/slugs) `GR(1)` synthesis tool.
Once installed, you can tell salty where to find the slugs executable by passing
the `-s` or `--slugs` flag.

### Building

Once `stack` and `slugs` are installed, salty can be built:

```shell
$ stack build
$ stack exec -- salty -h
```

Optionally, you can install salty globally using the `stack install` command,
which will place the salty binary in `$HOME/.local/bin`.

The examples can be built by running `make` in the `examples/` directory, and
will place all of the generated Java code in the `examples/build` directory.
