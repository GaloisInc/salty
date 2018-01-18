[![Build Status](https://travis-ci.org/GaloisInc/salty.svg?branch=wip%2Fspeedup-build)](https://travis-ci.org/GaloisInc/salty)

# The Salty DSL

Salty is a domain specific language for describing `GR(1)` synthesis problems.

## Current State

Salty is currently under active development, and can be used to produce Python,
Java, and C++ source from a `GR(1)` specification.

## Language Goals

* Make controller specifications easier to read by
  - Allowing macro definitions to give a name to complex behavior
  - Adding syntactic-sugar for things like `if-then-else`
  - Allowing state variables to range over values of an enumeration, rather than
    just integer values
* Integrate with the slugs `GR(1)` synthesizer for the heavy lifting
* Generate controller implementations in Python, Java and C++.

## Building Salty

### Dependencies

#### Stack

It's easiest to build salty using the [stack][stack] tool.  Stack will manage
the installation of all haskell dependencies, as well as the GHC compiler
itself. One downside to using `stack` is that it won't automatically pull
changes from the `language-slugs` repository.

#### Slugs

You will need to build and install the [slugs `GR(1)` synthesis tool][slugs].
Once installed, you can tell salty where to find the slugs executable by passing
the `-s` or `--slugs` flag. There is no installation target in the slugs
Makefile, however the `slugs` executable has no runtime dependencies, and can
just be copied into your `$PATH`.

#### Z3

Salty will do some additional sanity checking of specifications before sending
them to slugs. On linux, z3 is likely available in your package manager, and on
OSX it's available through [homebrew][homebrew]

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

[stack]: http://docs.haskellstack.org/en/stable/README/#how-to-install
[slugs]: https://github.com/VerifiableRobotics/slugs
[homebrew]: https://brew.sh
