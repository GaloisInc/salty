% The Salty Language

\pagebreak

# Introduction

Salty is a strongly-typed language for aiding GR(1) synthesis that provides a
front-end to the [Slugs][1] synthesis tool.


# Syntax

## Lexical Structure

Salty specifications are organized as a collection of top-level declarations,
all belonging to a single `controller` specification. The declarations are
assumed to be un-ordered, so it's perfectly reasonable to use a variable or
macro before its declaration.

## Controllers

The main compilation unit in Salty is the `controller`. A `controller` consists
of a types, state variables, macro definitions, and environment/system
constraint formulae. When synthesis is successful for a controller
specification, it takes the form of a function that accepts values for all of
the input variables, and produces values for all of the output variables.

The syntax for controllers is specified by the following grammar fragment:

```ebnf
Identifier      = lower-case-letter { IdentifierRest } ;
UpperIdentifier = upper-case-letter { IdentifierRest } ;

IdentifierRest  = letter | '0' - '9' | '_' ;

Controller      = 'controller' UpperIdentifier 'where' TopDecls ;
```

### Aside: Layout

Salty programs use indentation, rather than delimiters to indicate scoping. For
example, when introducing system transition formulas, `a`, `b`, and `c` are all
within the scope of the `sys_trans` block, while `d` is not.

```
sys_trans
  a
  b
  c

d
```

Anywhere that indentation is significant will be explicitly called out in the
remainder of this document. Additionally, in any grammar fragment where layout
is assumed to separate terminals, the terminals will be separated by a `'v;'`
terminal.

## Top-level Declarations

All of the top-level declarations in salty are subject to layout. For example,
the following program fragment will cause a parse error:

```
sys_trans a
  input b : Bool
```

as the sys_trans and input declarations do not start at the same column. As long
as they're consistent, the specification should parse; this is a valid fragment:

```
sys_trans a
input b : Bool
```

as is this:

```
  sys_trans a
  input b : Bool
```

The top-level declaration grammar is determined by the following grammar
fragment:

```ebnf
TopDecls = TopDecl | TopDecl 'v;' TopDecls | <empty> ;

TopDecl = InputDecl | OutputDecl | EnumDecl | Constraint | MacroDecl ;
```

### Types

The type-language of Salty consists of a few built-in types, and user-defined
enumerations. The built-in types are: the function arrow `->`, `Bool`, `Int`,
and `Spec`. User-defined enumerations will be introduced in the following
section.

* `Bool` is the type of boolean-valued expressions, like the constants True and
False, as well as inputs and outputs the are introduced with the `Bool` type.
* `-> is the type of user-defined macros, and any built-in functions.
* `Int` is the type of numeric expressions.
* `Spec` is the type of constraints, which will be defined in the [Constraints]
  section.

The types are described by the following grammar:

```ebnf
Type = 'Bool' | 'Int' | UpperIdentifier | Type '->' Type
```

### Enumerations

Enumerations allow users to introduce a new type and its values. The syntax for
introducing an enumeration type is shown below.

```ebnf
EnumDecl = 'enum' UpperIdent '=' UpperIdent { '|' UpperIdent }
```

For example, if you needed to express a number of different locations as an
input to your controller, this would be one way to describe that scenario:

```
enum Locations = Home | Work | InTransit

input current_location : Locations
```

With this controller fragment, the user may write constraints about the value of
`current_location`, with the confidence that it is always one of the values of
the `Location` enumeration (`Home`, `Work`, or `InTransit`).


### Expressions

Expressions can be constants, variables, macro invocations or combinations of
those using built-in operators.

```ebnf
Exprs = Expr | Expr 'v;' Exprs

Constant = 'True' | 'False' | integer | UpperIdent

Expr = Identifier | Number | Constant | Identifier '(' ExprArgs ')'
     | '{' ExprArgs '}' | 'mutex' Expr | 'all' Expr | 'any' Expr
     | Expr Op Expr | '!' Expr
     | 'if' Expr 'then' Expr 'else' Expr

Op = '||' | '\/' | '&&' | '/\' | '==' | '->' | '<->'

ExprArgs = Expr | Expr ',' Args
```

For example, if there is one boolean input, and one boolean output, and you wish
to make the value of the output the same as the value of the input, the
following controller fragment would achieve that goal:

```
sys_trans inpVar <-> outVar
```

#### If-then-else

The `if-then-else` syntax that Salty provides is just syntactic sugar for the
following expression:

```
if a then b else c == (a -> b) /\ (! a -> c)
```

In some cases, this construct can increase the readability of a specification,
but there is no special handling associated with it.

### Inputs and Outputs

Input and output declarations define how the controller can interact with the
world. Inputs and outputs both follow the same basic structure, but differ in
the keyword used to introduce them. The general form is shown below:

```ebnf
InputDecl = ( 'input' | 'output' ) Identifier ':' Type [ InitValue ]

InitValue = '=' Expr
```

Both inputs and outputs have optional initial values, which allows the user to
constrain the initial state of the state machine produced. If the initial values
are left unspecified for either, the resulting state machine can be a little
more complicated.

### Constraints

The behavior of the desired state machine is described by adding safety and
liveness constraints on either the environment or system, as top-level
declarations. Those declarations take the form shown below:

```ebnf
Constraint = RawConstraint | Ident '(' 
( 'sys_trans' | 'sys_liveness' | 'env_trans' | 'env_liveness' ) Exprs ;
```

For example, if you had an input variable named `a` and an output variable named
`b` that were both `Bool`-typed, you could enforce this through the following
system safety constraint:

```
sys_trans a == b
```

### Macros

As there can sometimes be quite a lot of repetition in specifications, Salty
supports user-defined macros to name common patterns. The basic structure of a
macro is given by the following grammar fragment:

```ebnf
MacroDecl = 'def' '(' ArgList ')' '=' (Expr | Constraints) ;

ArgList = Identifier | Identifier ',' ArgList | <empty> ;

Constraints = Constraint | Constraint 'v;' Constraints ;
```

As you can see from the grammar, the RHS of a macro definition can be either a
single expression, or one or more [Constraints]. The reason for this is that
macros can be used in two places:

* In an expression, where it is being used to compute a single value
* At the top-level, where it is used to name a pattern of constraints

The second case is useful for giving a name to a complicated property. For
example, if you are designing a command for an autonomous agent, and notice that
many commands have a similar structure, you can abstract out the parts that
differ, and use one top-level macro invocation instead. For example:

```
def action(enabled, loc, healthy, behavior) =
  sys_trans
    (enabled /\ healthy) -> (behavior_out == behavior /\ location_out == loc)

  sys_liveness
    enabled -> behavior_out == behavior

action(command_var == FindTrash, Hallway, BatteryOK == True, Search)
action(command_var == DeliverTrash, Garage, BatteryOK == True, Transport)
```

This way, the intent of the `sys_trans` and `sys_liveness` properties is encoded
in the name of the macro, `action`. Then, it can be used in place of writing
those formulae, but instantiated to their specific purpose.


# Additional Details

## Enumerations

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


# Debugging

During specification development, you will likely encounter situations where the
specification is not realizable. Salty can help a little bit with this, by
identifying a few situations that will cause slugs to reject your specification,
before realizability is checked.

## Unsatisfiable Safety Constraints

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


## Unsatisfiable initial constraints

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


## Liveness properties that eventually violate safety

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
