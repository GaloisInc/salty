#!/usr/bin/env python
"""
Usage: genGnomesSpec.py [-p] [-t N] [-g G] [H W]

Generates a Salty spec for a random gridworld of height H and width W (default is 5x10),
with N trolls (default 1) at random positions, G goals (default 2) at random positions,
and saves the resulting specification in file randGrid_T<N>_G<G>_H<H>_W<W>.salt

Troll region radii are set using the variable TROLL_RADIUS (default 1).

If the flag "-p" is given, then generate a plot of the grid.

"""
from __future__ import print_function

import sys
import tulip.gridworld as gw
from tulip.synth import is_realizable
import re


def main(argv):

    TROLL_RADIUS = 1

    if len(sys.argv) > 8 or "-h" in sys.argv:
        print("Usage: genGnomesSpec.py [-p] [-t N] [-g G] [H W]")
        sys.exit(1)

    try:
        targ_ind = sys.argv.index("-t")
    except ValueError:
        targ_ind = -1
    num_trolls = 1 if targ_ind < 0 else int(sys.argv[targ_ind+1])

    try:
        garg_ind = sys.argv.index("-g")
    except ValueError:
        garg_ind = -1
    num_goals = 2 if garg_ind < 0 else int(sys.argv[garg_ind+1])

    if "-p" in sys.argv:
        print_pretty = True
    else:
        print_pretty = False

    if len(sys.argv) >= 3 and sys.argv[-2][0] != "-" and sys.argv[1][0] != "-":
        (height, width) = (int(sys.argv[-2]), int(sys.argv[-1]))
    else:
        (height, width) = (5, 10)

    realizable = False
    while not realizable:
        Z, troll_list = gw.random_world((height, width), wall_density=0.2, num_init=1, num_goals=num_goals,
                                        num_trolls=num_trolls)
        for i in range(len(troll_list)):
            troll_list[i] = (troll_list[i][0], TROLL_RADIUS)
        (spec, moves_N) = gw.add_trolls(Z, troll_list)
        spc = Z.spec()
        spc.moore = False
        spc.qinit = r'\A \E'
        if not is_realizable(spc):
            print("Random spec not realizable. Trying again ...")
        else:
            realizable = True

    print(Z.pretty(show_grid=True, line_prefix="## "))
    print(Z.dumps(line_prefix="# "))
    print(spec.pretty())

    # Produce salty specification
    salty_file = open('grid_T' + str(num_trolls) + '_G' + str(num_goals) + '_H' + str(height) + '_W' + str(width) +
                      '.salt', 'w')
    salty_string = 'controller RandGrid where\n\n'

    # Create enumerations for environment variables
    for i in range(0, len(spec.env_vars)/2):
        num_cols = spec.env_vars['X_' + str(i) + '_c'][1]
        salty_string += 'enum E' + str(i) + 'c = '
        for j in range(0, num_cols):
            salty_string += 'X' + str(i) + '_c' + str(j) + ' | '
        salty_string += 'X' + str(i) + '_c' + str(num_cols) + '\n'

        num_rows = spec.env_vars['X_' + str(i) + '_r'][1]
        salty_string += 'enum E' + str(i) + 'r = '
        for j in range(0, num_rows):
            salty_string += 'X' + str(i) + '_r' + str(j) + ' | '
        salty_string += 'X' + str(i) + '_r' + str(num_rows) + '\n'

    # Create enumerations for system variables
    num_cols = spec.sys_vars['Y_c'][1]
    salty_string += 'enum Sc = '
    for i in range(0, num_cols):
        salty_string += 'Y_c' + str(i) + ' | '
    salty_string += 'Y_c' + str(num_cols) + '\n'

    num_rows = spec.sys_vars['Y_r'][1]
    salty_string += 'enum Sr = '
    for i in range(0, num_rows):
        salty_string += 'Y_r' + str(i) + ' | '
    salty_string += 'Y_r' + str(num_rows) + '\n\n'

    # Set initial environment values
    for i in range(0, len(spec.env_vars)/2):
        formula = spec.env_init[i]
        col_number = re.findall('X_' + str(i) + '_c\s*=\s*(\d+)', formula)[0]
        salty_string += 'input x' + str(i) +'c : E' + str(i) + 'c = X' + str(i) + '_c' + col_number + '\n'
        row_number = re.findall('X_' + str(i) + '_r\s*=\s*(\d+)', formula)[0]
        salty_string += 'input x' + str(i) + 'r : E' + str(i) + 'r = X' + str(i) + '_r' + row_number + '\n'

    # Set initial system values
    formula = spec.sys_init[0]
    col_number = re.findall('Y_c\s*=\s*(\d+)', formula)[0]
    salty_string += 'output yc : Sc = Y_c' + str(col_number) + '\n'
    row_number = re.findall('Y_r\s*=\s*(\d+)', formula)[0]
    salty_string += 'output yr : Sr = Y_r' + str(row_number) + '\n\n'

    # Set environment transitions
    salty_string += 'env_trans\n'
    for formula in spec.env_safety:
        formula = ConvertFormula(formula)
        salty_string += '  ' + formula + '\n'
    salty_string += '\n'

    # Set environment liveness
    for formula in spec.env_prog:
        salty_string += 'env_liveness\n'
        formula = ConvertFormula(formula)
        salty_string += '  ' + formula + '\n\n'

    # Set system transitions
    salty_string += 'sys_trans\n'
    for formula in spec.sys_safety:
        formula = ConvertFormula(formula)
        salty_string += '  ' + formula + '\n'
    salty_string += '\n'

    # Set system liveness
    for formula in spec.sys_prog:
        salty_string += 'sys_liveness\n'
        formula = ConvertFormula(formula)
        salty_string += '  ' + formula + '\n\n'

    salty_file.write(salty_string)

    salty_file.close()


def ConvertFormula(formula):

    def ConvertNext(formula):
        # Search for instances of the next operator
        p = re.compile("X\s*(\()", re.IGNORECASE)
        m = p.search(formula)
        has_paren = True
        if m is None:
            p = re.compile("X\s+", re.IGNORECASE)
            m = p.search(formula)
            has_paren = False

        # If there are none, return the unmodified formula
        if m is None:
            return formula

        # If there are, process the formula, eating terms from formula as you go
        processed_formula = ""
        while m is not None:
            processed_formula += formula[:m.start()]
            index = m.end()
            if has_paren:
                num_paren = 1
                while True:
                    if formula[index] == '(':
                        num_paren += 1
                    elif formula[index] == ')':
                        num_paren -= 1

                    if num_paren == 0:
                        break;
                    else:
                        index += 1
                term = formula[m.end():index]
            else:
                m2 = re.match("[^=]+=\s*\d+", formula)
                term = formula[m.end():m2.end()]
                index = m2.end()
            term = re.sub("([a-zA-Z]\w+)", r"\1'", term)
            processed_formula += term
            formula = formula[index + 1:] if has_paren else formula[index:]

            p = re.compile("X\s*(\()", re.IGNORECASE)
            m = p.search(formula)
            has_paren = True
            if m is None:
                p = re.compile("X\s+", re.IGNORECASE)
                m = p.search(formula)
                has_paren = False

        processed_formula += formula
        return processed_formula

    def ConvertVariables(formula):
        formula = re.sub("[X|x]_(\d+)_(r|c)('?)", r"x\1\2\3", formula)
        formula = re.sub("[Y|y]_(r|c)('?)", r"y\1\2", formula)
        return formula

    def ConvertEnumerations(formula):
        formula = re.sub("x(\d+)(r|c)('?)\s*=\s*(\d+)",r"x\1\2\3 == X\1_\2\4", formula)
        formula = re.sub("y(r|c)('?)\s*=\s*(\d+)", r"y\1\2 == Y_\1\3", formula)
        return formula

    formula = ConvertNext(formula)
    formula = ConvertVariables(formula)
    formula = ConvertEnumerations(formula)
    # The grammar is supposed to use && and ||, but sometimes it uses & and |
    formula = formula.replace(r'&& ', r'/\ ')
    formula = formula.replace(r'& ', r'/\ ')
    formula = formula.replace(r'||', r'\/')
    formula = formula.replace(r'|', r'\/')

    return formula


if __name__ == "__main__":
   main(sys.argv[1:])