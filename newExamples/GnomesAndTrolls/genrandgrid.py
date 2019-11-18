#!/usr/bin/env python
"""
Usage: genrandgrid.py [-p] [-i FILE] [-t N] [-g G] [-b] [H W]

will generate a random gridworld of height H and width W (default is
5x10), with N trolls (default 1) at random positions, G goals (default 1)
at random positions, and print the resulting specification.

Troll region radii are set using the variable TROLL_RADIUS (default 1).

If the flag "-p" is given, then generate a plot of the grid.  If the -i
flag is given in addition to -p, then save the plot to a PDF FILE.  If the
flag -b is given, instead of integer-valued variables, use representation
where there is one boolean variable in the specification per grid cell.

LRH 30 Aug 2019:
Modified from genrandt.py in TuLiP repository to produce a Salty specification.
Tries to ensure all specifications are realizable, but doesn't always work
for some reason, especially if G > 1. May be doing weird things with encoding 
the generated grid. Or maybe it's generating multiple gnomes too or something.

"""
from __future__ import print_function

import sys
import tulip.gridworld as gw
from tulip.synth import is_realizable
import re


TROLL_RADIUS = 1

if len(sys.argv) > 11 or "-h" in sys.argv:
    print("Usage: genrandt.py [-p] [-i FILE] [-t N] [-g G] [-b] [H W]")
    sys.exit(1)

if "-b" in sys.argv:
    nonbool = False
    sys.argv.remove("-b")
else:
    nonbool = True

try:
    targ_ind = sys.argv.index("-t")
    if targ_ind > len(sys.argv)-2:
        print("Invalid use of -t flag.  Try \"-h\"")
        sys.exit(1)
except ValueError:
    targ_ind = -1
if targ_ind < 0:
    N = 1
else:
    N = int(sys.argv[targ_ind+1])

try:
    garg_ind = sys.argv.index("-g")
    if garg_ind > len(sys.argv)-2:
        print("Invalid use of -g flag.  Try \"-h\"")
        sys.exit(1)
except ValueError:
    garg_ind = -1
if garg_ind < 0:
    num_goals = 1
else:
    num_goals = int(sys.argv[garg_ind+1])

if "-p" in sys.argv:
    print_pretty = True
    sys.argv.remove("-p")
    try:
        iarg_ind = sys.argv.index("-i")+1
        if iarg_ind > len(sys.argv)-1:
            print("Invalid use of -i flag.  Try \"-h\"")
            sys.exit(1)
    except ValueError:
        iarg_ind = -1
else:
    print_pretty = False

if len(sys.argv) >= 3 and sys.argv[-2][0] != "-":
    (height, width) = (int(sys.argv[-2]), int(sys.argv[-1]))
else:
    (height, width) = (2, 3)

realizable = False

while not realizable:

    Z, troll_list = gw.random_world((height, width), wall_density=0.2, num_init=1, num_goals=num_goals, num_trolls=N)

    for i in range(len(troll_list)):
        troll_list[i] = (troll_list[i][0], TROLL_RADIUS)

    (spec, moves_N) = gw.add_trolls(Z, troll_list)

    spc = Z.spec()
    spc.moore = False
    spc.qinit = r'\A \E'
    if not is_realizable(spc):
        print("Not realizable.")
    else:
        realizable = True

print(Z.pretty(show_grid=True, line_prefix="## "))

print(Z.dumps(line_prefix="# "))

print(spec.pretty())

if print_pretty:
    Z.plot(font_pt=0, troll_list=troll_list)
    if iarg_ind == -1:
        plt.show()
    else:
        plt.savefig(sys.argv[iarg_ind])

# Addition to produce salty specification
salty_file = open('grid_' + str(height) + '_' + str(width) + '.salt', 'w')
salty_file.write('controller RandGrid where\n\n')
max_input_col = spec.env_vars['X_0_c'][1]
max_input_row = spec.env_vars['X_0_r'][1]
max_output_col = spec.sys_vars['Y_c'][1]
max_output_row = spec.sys_vars['Y_r'][1]

salty_file.write('enum Ec = ')
for i in range(0, max_input_col):
    salty_file.write('X' + str(i) + 'c | ')
salty_file.write('X' + str(max_input_col) + 'c\n')

salty_file.write('enum Er = ')
for i in range(0, max_input_row):
    salty_file.write('X' + str(i) + 'r | ')
salty_file.write('X' + str(max_input_row) + 'r\n')

salty_file.write('enum Sc = ')
for i in range(0, max_output_col):
    salty_file.write('Y' + str(i) + 'c | ')
salty_file.write('Y' + str(max_output_col) + 'c\n')

salty_file.write('enum Sr = ')
for i in range(0, max_output_col):
    salty_file.write('Y' + str(i) + 'r | ')
salty_file.write('Y' + str(max_output_col) + 'r\n\n')

salty_file.write('input xc : Ec = X' + re.search("X_0_c\s+=\s+(\d+)", spec.env_init[0]).group(1) + 'c\n')
salty_file.write('input xr : Er = X' + re.search("X_0_r\s+=\s+(\d+)", spec.env_init[0]).group(1) + 'r\n')
salty_file.write('output yc : Sc = Y' + re.search("Y_r\s+=\s+(\d+)", spec.sys_init[0]).group(1) + 'c\n')
salty_file.write('output yr : Sr = Y' + re.search("Y_c\s+=\s+(\d+)", spec.sys_init[0]).group(1) + 'r\n')

salty_file.write('\nenv_trans\n')

for env_trans in spec.env_safety:
    env_trans = re.sub('X_0_(r|c)', r"x\1", env_trans)
    env_trans = re.sub('\(x(r|c) = (\d+)\)', r"x\1 == X\2\1", env_trans)
    env_trans = re.sub('\(X\s+(x[r|c])([^)]+)\)', r"\1'\2", env_trans)
    env_trans = env_trans.replace(r"||", r"\/")
    env_trans = env_trans.replace(r"&& ", r"/\ ")
    env_trans = env_trans.replace(r"& ", r"/\ ")
    salty_file.write("  " + env_trans + "\n")

salty_file.write('\nenv_liveness\n')

for env_live in spec.env_prog:
    env_live = re.sub('X_0_(r|c)', r"x\1", env_live)
    env_live = re.sub('\(x(r|c) = (\d+)\)', r"x\1 == X\2\1", env_live)
    env_live = re.sub('\(X\s+(x[r|c])([^)]+)\)', r"\1'\2", env_live)
    env_live = env_live.replace(r"||", r"\/")
    env_live = env_live.replace(r"&& ", r"/\ ")
    env_live = env_live.replace(r"& ", r"/\ ")
    salty_file.write("  " + env_live + "\n")

salty_file.write('\nsys_trans\n')

for sys_trans in spec.sys_safety:
    sys_trans = re.sub('Y_(r|c)', r"y\1", sys_trans)
    sys_trans = re.sub('\(y(r|c) = (\d+)\)', r"y\1 == Y\2\1", sys_trans)
    sys_trans = re.sub('\(X\s+(y[r|c])([^)]+)\)', r"\1'\2", sys_trans)
    sys_trans = re.sub('X_0_(r|c)', r"x\1", sys_trans)
    sys_trans = re.sub('x(r|c) = (\d+)', r"x\1 == X\2\1", sys_trans)
    sys_trans = re.sub('\(X\s+(x[r|c])([^)]+)\)', r"\1'\2", sys_trans)
    sys_trans = sys_trans.replace(r"||", r"\/")
    sys_trans = sys_trans.replace(r"&& ", r"/\ ")
    sys_trans = sys_trans.replace(r"& ", r"/\ ")
    salty_file.write("  " + sys_trans + "\n")

salty_file.write('\nsys_liveness\n')

for sys_live in spec.sys_prog:
    sys_live = re.sub('Y_(r|c)', r"y\1", sys_live)
    sys_live = re.sub('\(y(r|c) = (\d+)\)', r"y\1 == Y\2\1", sys_live)
    sys_live = re.sub('\(X\s+(y[r|c])([^)]+)\)', r"\1'\2", sys_live)
    sys_live = re.sub('X_0_(r|c)', r"x\1", sys_live)
    sys_live = re.sub('\(x(r|c) = (\d+)\)', r"x\1 == X\2\1", sys_live)
    sys_live = re.sub('\(X\s+(x[r|c])([^)]+)\)', r"\1'\2", sys_live)
    sys_live = sys_live.replace(r"||", r"\/")
    sys_live = sys_live.replace(r"&& ", r"/\ ")
    sys_live = sys_live.replace(r"& ", r"/\ ")
    salty_file.write("  " + sys_live + "\n")

salty_file.close()
