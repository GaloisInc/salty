#!/usr/bin/python

# This script converts files from Anzu to Salty.

# Assumptions:
# * There is only one section for each of [INPUT_VARIABLES], [OUTPUT_VARIABLES], etc.
# * Files do not have syntax errors (there is no robust error checking here)
# * Transition and liveness formulas can be broken across lines
# * Variable names and multi-character operators are not broken across lines (->, <->)
# * Since salty requires input and output names to be all lowercase, this script
#   changes all names to lowercase and checks that they are still unique

import getopt
import sys
import re


def main(argv):
    # Process input arguments
    try:
        opts, args = getopt.getopt(argv, "h", [])
    except getopt.GetoptError:
        print('usage: psltosalty.py input_filename output_filename')
        sys.exit()
    if len(args) < 3:
        print('usage: psltosalty.py input_filename output_filename controller_name')
        sys.exit()
    input_filename = args[0]
    output_filename = args[1]
    controller_name = args[2]
    try:
        input_file = open(input_filename, "r")
    except:
        print('error: could not open file ' + args[0])
        print('error details:')
        for info in sys.exc_info():
            print(str(sys.exc_info()))
        sys.exit()
    try:
        output_file = open(output_filename, "w")
    except:
        print('error: could not open file ' + args[0])
        print('error details:')
        for info in sys.exc_info():
            print(str(sys.exc_info()))
        sys.exit()

    # Read in the file
    contents = input_file.read()

    # Remove all comments that start a line
    contents = re.sub("\n#.*", "", contents)
    contents = re.sub("^\s*#.*\n", "", contents)

    # Remove all comments that end a line
    contents = re.sub("#[^\n]*", "", contents)

    # Convert all ands, ors, terms=0 (nots), and terms=1
    contents = contents.replace("*", "/\\")
    contents = contents.replace("+", "\/")
    contents = re.sub("(\w+)\s*=\s*0", r'!\1', contents)
    contents = re.sub("(\w+)\s*=\s*1", r'\1', contents)

    # Write controller name
    output_file.write("controller " + controller_name + " where\n\n")

    # Create a dictionary of variable names to ensure that converting names to lowercase
    # does not result in a collision
    variable_names = set()

    # Handle all input declarations
    p = re.compile("\[INPUT_VARIABLES\]([^\[]*)", re.IGNORECASE)
    m = p.search(contents)
    var_names_string = m.group(1)
    var_name_list = re.findall("(\w+)", var_names_string)
    for var_name in var_name_list:
        if var_name in variable_names:
            print("Error -- Duplicate variable name: " + var_name)
            sys.exit()
        else:
            variable_names.add(var_name)
            output_file.write("input " + str.lower(var_name) + " : Bool\n");
    output_file.write("\n");

    # Handle all output declarations
    p = re.compile("\[OUTPUT_VARIABLES\]([^\[]*)", re.IGNORECASE)
    m = p.search(contents)
    var_names_string = m.group(1)
    var_name_list = re.findall("(\w+)", var_names_string)
    for var_name in var_name_list:
        if var_name in variable_names:
            print("Error -- Duplicate variable name: " + var_name)
            sys.exit()
        else:
            variable_names.add(var_name)
            output_file.write("output " + str.lower(var_name) + " : Bool\n");
    output_file.write("\n");

    # Handle all initial environment conditions
    output_file.write("env_init\n");
    p = re.compile("\[ENV_INITIAL\]([^\[]*)", re.IGNORECASE)
    m = p.search(contents)
    init_string = m.group(1)
    term_list = re.findall("(!?\w+)", init_string)
    if len(term_list) > 0:
        for term in term_list:
            output_file.write("  " + str.lower(term) + "\n");
    else:
        output_file.write("  True\n");
    output_file.write("\n");

    # Handle all environment transition specs
    output_file.write("env_trans\n");
    p = re.compile("\[ENV_TRANSITIONS\]([^\[]*)", re.IGNORECASE)
    m = p.search(contents)
    trans_string = m.group(1)
    formula_list = re.split("[\s]*;[\s]*", trans_string)
    if len(formula_list) > 1:
        for formula in formula_list:
            if len(formula) > 0:
                formula = process_transition_formula(formula)
                output_file.write("  " + case_all_terms(formula) + "\n")
    else:
        output_file.write("  True");
    output_file.write("\n");

    # Handle all environment liveness specs
    p = re.compile("\[ENV_FAIRNESS\]([^\[]*)", re.IGNORECASE)
    m = p.search(contents)
    liveness_string = m.group(1)
    formula_list = re.split("[\s]*;[\s]*", liveness_string)
    if len(formula_list) > 1:
        for formula in formula_list:
            if len(formula) > 0:
                output_file.write("env_liveness\n");
                formula = process_liveness_formula(formula)
                output_file.write("  " + case_all_terms(formula) + "\n\n")
    else:
        output_file.write("env_liveness\n");
        output_file.write("  True\n\n");

    # Handle all initial system conditions
    output_file.write("sys_init\n");
    p = re.compile("\[SYS_INITIAL\]([^\[]*)", re.IGNORECASE)
    m = p.search(contents)
    init_string = m.group(1)
    term_list = re.findall("(!?\w+)", init_string)
    if len(term_list) > 0:
        for term in term_list:
            output_file.write("  " + str.lower(term) + "\n");
    else:
        output_file.write("  True\n");
    output_file.write("\n");

    # Handle all system transition specs
    output_file.write("sys_trans\n");
    p = re.compile("\[SYS_TRANSITIONS\]([^\[]*)", re.IGNORECASE)
    m = p.search(contents)
    trans_string = m.group(1)
    formula_list = re.split("[\s]*;[\s]*", trans_string)
    if len(formula_list) > 1:
        for formula in formula_list:
            if len(formula) > 0:
                formula = process_transition_formula(formula)
                output_file.write("  " + case_all_terms(formula) + "\n")
    else:
        output_file.write("  True");
    output_file.write("\n");

    # Handle all system liveness specs
    p = re.compile("\[SYS_FAIRNESS\]([^\[]*)", re.IGNORECASE)
    m = p.search(contents)
    liveness_string = m.group(1)
    formula_list = re.split("[\s]*;[\s]*", liveness_string)
    if len(formula_list) > 1:
        for formula in formula_list:
            if len(formula) > 0:
                output_file.write("sys_liveness\n");
                formula = process_liveness_formula(formula)
                output_file.write("  " + case_all_terms(formula) + "\n\n")
    else:
        output_file.write("sys_liveness\n");
        output_file.write("  True\n\n");

    output_file.close()


def process_transition_formula(formula):
    # Remove all newlines, then remove extra whitespace (including beginning of string)
    formula = re.sub("\n", "", formula)
    formula = re.sub("\s+", " ", formula)
    formula = re.sub("^\s*", "", formula)

    # Check that the formula is in fact a transition formula and remove outer tokens
    # 'G(' and ')' if necessary
    if str.lower(formula) == "true":
        return "True"
    elif str.lower(formula) == "false":
        return "False"
    elif str.lower(formula[0:2]) == "g(" and formula[-1] == ")":
        formula = formula[2:-1]
        # Handle all next 'X' operators by putting ticks on variables
        formula = process_all_x_terms(formula)
        return formula
    else:
        print("Error -- Invalid transition formula: " + formula)
        sys.exit()


def process_liveness_formula(formula):
    # Remove all newlines, then remove extra whitespace (including beginning of string)
    formula = re.sub("\n", "", formula)
    formula = re.sub("\s+", " ", formula)
    formula = re.sub("^\s*", "", formula)

    # Check that the formula is in fact a liveness formula and remove outer tokens
    # 'G(F(' and ')' if necessary
    if str.lower(formula) == "true":
        return "True"
    elif str.lower(formula) == "false":
        return "False"
    elif str.lower(formula[0:4]) == "g(f(" and formula[-2:] == "))":
        formula = formula[4:-2]
        # Handle all next 'X' operators by putting ticks on variables
        formula = process_all_x_terms(formula)
        return formula
    else:
        print("Error -- Invalid liveness formula: " + formula)
        sys.exit()


def process_all_x_terms(formula):
    p = re.compile("X\(([^\)]+)\)")
    m = p.search(formula)
    while m is not None:
        formula_head = formula[0:m.span()[0]]
        term = formula[m.span()[0]+2:m.span()[1]-1]  # Remove 'X(' and ')'
        formula_tail = formula[m.span()[1]:]
        term = re.sub("(\w+)", r"\1'", term)
        formula = formula_head + term + formula_tail
        m = p.search(formula)
    return formula


def case_all_terms(formula):
    p = re.compile("(\w+)")
    m_list = p.finditer(formula)
    for m in m_list:
        if str.lower(formula[m.span()[0]:m.span()[1]]) == "false":
            formula = formula[:m.span()[0]] + "False" + formula[m.span()[1]:]
        elif str.lower(formula[m.span()[0]:m.span()[1]]) == "true":
            formula = formula[:m.span()[0]] + "True" + formula[m.span()[1]:]
        else:
            formula = formula[:m.span()[0]] + str.lower(formula[m.span()[0]:m.span()[1]]) + formula[m.span()[1]:]
    return formula


if __name__ == "__main__":
   main(sys.argv[1:])
