Pulled from https://github.com/tulip-control/tulip-control/tree/master/examples/gridworlds

Usage: genrandgrid.py [-p] [-i FILE] [-t N] [-g G] [-b] [H W]

will generate a random gridworld of height H and width W (default is
5x10), with N trolls (default 1) at random positions, G goals (default 1)
at random positions, and print the resulting specification.

Troll region radii are set using the variable TROLL_RADIUS (default 1).

If the flag "-p" is given, then generate a plot of the grid.  If the -i
flag is given in addition to -p, then save the plot to a PDF FILE.  If the
flag -b is given, instead of integer-valued variables, use representation
where there is one boolean variable in the specification per grid cell.

LRH 3 Sep 2019: Does not always produce realizable specs.
Changed default number of goals to 1, since more goals results in strange specifications.