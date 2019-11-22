# Gnomes and Trolls
Originally based on genrandt.py pulled from
[tulip](https://github.com/tulip-control/tulip-control/tree/master/examples/gridworlds)

Usage:
```
python2 genrandgrid.py [-p] [-t N] [-g G] [H W]
```

Generates a Salty spec for a random gridworld of height H and width W (default is 5x10), 
with N trolls (default 1) at random positions, G goals (default 2) at random positions, 
and saves the resulting specification in file grid_T<N>_G<G>_H<H>_W<W>.salt

If the flag "-p" is given, then generate a plot of the grid.

Troll region radii can be set inside the script using the variable TROLL_RADIUS (default 1).

## Requirements
Requires tulip. You might be able simply download the tulip repository, then add
its location to your PYTHONPATH, e.g.
```
export PYTHONPATH="\home\username\tulip-control"
```
However, tulip itself has dependencies that you may also need to install.