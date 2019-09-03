Pulled from https://github.com/tulip-control/tulip-control/tree/master/examples

Modified their python program gr1.py to prettyprint the specification.
With TuLiP installed, run
  python2 gr1.py
to produce the following output, which was translated to Salty by hand:

ENVIRONMENT VARIABLES:
	park	boolean

SYSTEM VARIABLES:
	X0reach	boolean
	X2	boolean
	X3	boolean
	X0	boolean
	X1	boolean
	X4	boolean
	X5	boolean

FORMULA:
ASSUMPTION:
    LIVENESS
	  []<>(!park)
GUARANTEE:
    INITIAL
	  (X0)
	& (X0reach)
    SAFETY
	  [](X0 -> X (X1 || X3))
	& []((X (X0reach) <-> X0) || (X0reach && !park))
	& [](X5 -> X (X4 || X2))
	& [](X2 -> X (X1 || X5))
	& [](X4 -> X (X3 || X1 || X5))
	& []((((X2) && !(X3) && !(X0) && !(X1) && !(X4) && !(X5)) || ((X3) && !(X2) && !(X0) && !(X1) && !(X4) && !(X5)) || ((X0) && !(X2) && !(X3) && !(X1) && !(X4) && !(X5)) || ((X1) && !(X2) && !(X3) && !(X0) && !(X4) && !(X5)) || ((X4) && !(X2) && !(X3) && !(X0) && !(X1) && !(X5)) || ((X5) && !(X2) && !(X3) && !(X0) && !(X1) && !(X4))))
	& [](X3 -> X (X0 || X4))
	& [](X1 -> X (X0 || X4 || X2))
    LIVENESS
	  []<>(X0reach)
	& []<>(X5)
