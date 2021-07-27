[![Build Status](https://travis-ci.org/GaloisInc/salty.svg?branch=wip%2Fspeedup-build)](https://travis-ci.org/GaloisInc/salty)

# The Salty DSL

Salty is a domain specific language for describing `GR(1)` synthesis problems.

## Current State

Salty is currently under active development, and can be used to produce Python,
Java, C++, and SPARK/Ada source from a `GR(1)` specification.

## Language Goals

* Make controller specifications easier to read by
  - Allowing macro definitions to give a name to complex behavior
  - Adding syntactic-sugar for things like `if-then-else`
  - Allowing state variables to range over values of an enumeration, rather than
    just integer values
* Integrate with the slugs `GR(1)` synthesizer for the heavy lifting
* Generate controller implementations in Python, Java, C++, and SPARK/Ada

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
Makefile. However, the `slugs` executable has no runtime dependencies, so 
"installing" it by adding the executable's directory to your `$PATH` is 
sufficient for building `salty`.

#### Z3
Salty will do some additional sanity checking of specifications before sending
them to slugs. On linux, z3 is likely available in your package manager (e.g. 
`apt`) , and on OSX it's available through [homebrew][homebrew].

### Building
Once `stack`, `z3`, and `slugs` are installed, salty can be built:

```shell
$ stack build
$ stack exec -- salty -h
```

Optionally, you can install salty globally using the `stack install` command,
which will place the salty binary in `$HOME/.local/bin`. Add this directory to
your `$PATH` if you want to be able to run `salty` from an arbitrary directory.

Test examples can be built by running `make` in the `tests/` directory, and
will place all of the generated Java code in the `tests/build` directory.

Additional example `.salt` files are available in the `examples` directory.

[stack]: http://docs.haskellstack.org/en/stable/README/#how-to-install
[slugs]: https://github.com/VerifiableRobotics/slugs
[homebrew]: https://brew.sh
