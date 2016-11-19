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

The initial values of the variables is optional, so the same example with
variables that are unconstrained in the initial state just omits the `= False`
syntax:

```
input  a : Bool
output b : Bool
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


## Debugging

During specification development, you will likely encounter situations where the
specification is not realizable. Salty can help a little bit with this, by
identifying a few situations that will cause slugs to reject your specification,
before realizability is checked.

### Unsatisfiable Safety Constraints

As you can specify safety constraints on the environment and system in many
different places, it's easy to end up with an unsatisfiable specification. As an
example, consider the following specification:

```
controller Example where

input  x1: Bool
input  x2: Bool
output y1: Bool
output y2: Bool

env_trans   x1
env_trans ! x1

env_trans x2' == !x2

sys_trans   y1
sys_trans ! y1

sys_trans y2' == !y2
```

The sanity checker in salty will reject this specification for two reasons:

1. The environment safety properties are unsatisfiable. This by itself isn't a
   problem for slugs, as the semantics it uses allow it to produce a controller,
   as long as the system satisfies its safety properties for at least one step.
   However, the sanity checker in salty rejects this situation, as it produces a
   controller that would satisfy system safety for exactly one step, and then
   behave unpredictably forever.
2. The system safety properties are unsatisfiable. This is a problem, as slugs
   will refuse to synthesize a controller for a specification that will never be
   able to satisfy its safety properties.

Salty will output two errors in response to these problems:

```
-- [error] ---------------------------------------------------------------------
  Environment safety constraints are never satisfiable:
    * environment safety constraint at example.salt:8,13-8,15
    * environment safety constraint at example.salt:9,11-9,15

-- [error] ---------------------------------------------------------------------
  System safety constraints are never satisfiable
    * system safety constraint at example.salt:14,11-14,15
    * system safety constraint at example.salt:13,13-13,15
```

The errors attempt to report a minimal set of conflicting safety properties, to
aid specification debugging; the errors above don't mention the safety
properties on lines 11 and 15 because they are not contributing to the
unsatisfiability of the environment or system.


### Unsatisfiable initial constraints

Once salty has managed to determine that the safety properties of the system are
satisfiable, it will continue by checking the initial state of the controller.
If any of the initial value constraints for state variables cause the
environment or system safety properties to become unsatisfiable, an error is
raised.

Although slugs would be able to synthesize a controller whose variable
initialization violated environmental safety properties, salty considers that to
be an error. The reasoning for this is the same as when checking environmental
safety properties for satisfiability: if the controller produced would only
satisfy its system safety properties for a single time step, that is not a
useful controller.


### Liveness properties that eventually violate safety

Liveness properties in the system and environment can eventually cause safety
properties to be violated. Consider this example specification:

```
controller Example where

input a : Bool = False

env_trans ! a

env_liveness a
```

This specification will generate a warning, because it will eventually violate
its environmental safety properties. The reason that this isn't rejected is that
the semantics of slugs requires that the system be able to satisfy its safety
properties during the state transition when the environment violates its own.
All states after that are allowed to violate system safety.

If we augment the same example with a system safety property that will be
violated by a liveness property, this will also produce a warning, as salty is
not able to tell if the environment or the system would violate safety first. As
a result, this specification will still be sent to slugs, and may be
unrealizable.

However, if there is a liveness property that would cause a system safety
violation, and no environment safety violation can be established, the sanity
checker generates an error and salty rejects the specification.


[1]: https://github.com/VerifiableRobotics/slugs
