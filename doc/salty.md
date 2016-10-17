# Salty

Salty is a language for aiding GR(1) synthesis that provides a type-checked
front-end to the [Slugs][1] synthesis tool.

## Layout

TODO

## Top-level Declarations

### Inputs/Outputs

Inputs (environment variables) and outputs (system variables) can be defined
through the use of the `input` and `output` declarations, respectively. For
example, if we wanted to make a simple controller that had a single boolean
input and a single boolean output, both initialized to `False`, this controller
fragment would accomplish that:

```
input  a : Bool = False
output b : Bool = False
```

### Environment and System Safety/Liveness

Safety and liveness constraints can be put on either the environment or the
system, through use of the `env_trans`, `env_liveness`, `sys_trans`,
`sys_liveness` declarations. For example, if you would like to have the output
`b` always be equal to the input `a`, you can use the following declaration:

```
sys_trans b == a
```

Each of these top-level declarations accepts one or more boolean-typed
expressions as input.

### Enumerations

Sometimes, the inputs to a controller will be mutually exclusive. In this
scenario, salty provides two options:

1. You can use the `mutex` built-in function to show that at most one of a set
   of variables is allowed to be true
2. You can define a new enumeration for all of the values, and use one input
   that is typed as that enumeration

For the first approach, the resulting controller fragment looks like this:

```
input a : Bool
input b : Bool
input c : Bool

env_trans mutex {a, b, c}
```

This states that there are three boolean inputs, and that the environment will
only ever assert at most one of them. The `mutex` function accepts a set of
boolean-typed expressions, and produces a boolean-typed expression.

While this approach does work, it doesn't scale well; each input defined in a
controller adds to the complexity of the state machine that is produced. Salty
provides a way to retain the mutually exclusive characteristics of the input,
while lowering the number of variables required through the use of an
enumeration. Translating the example above yields:

```
enum T = A | B | C
input x : T
```

The choice of names here is arbitrary: `T` is just the name of the type of the
enumeration, and `A`, `B`, and `C` are just chosen to reflect the intent of the
original controller. The input name `x` is not significant.

This yields a controller that only requires two input variables, as the input
`x` is translated into a bit-vector of size two. The result is that as the
number of possible cases for an enumeration grow, the size of the bit-vector
grows at `log_2` of the number of constructors.




[1]: https://github.com/VerifiableRobotics/slugs
