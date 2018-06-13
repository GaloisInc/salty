#!/usr/bin/python
import sys
import getopt
import os
import re


def main(argv):
    # ================================================================================
    # Process input arguments
    # ================================================================================
    try:
        opts, args = getopt.getopt(argv, "h", [])
    except getopt.GetoptError:
        print('usage: CreateUxasSkeleton.py \033[4m' + 'input_file.salt' + '\033[0m')
        sys.exit()
    if len(args) < 1:
        print('usage: CreateUxasSkeleton.py \033[4m' + 'input_file.salt' + '\033[0m')
        sys.exit()
    try:
        salty_file = open(args[0], 'r')
    except:
        print('error: could not open file ' + args[0])
        print('error details:')
        print(str(sys.exc_info()))
        sys.exit()
    dir_inputs = os.path.relpath('./psaltlib/Inputs')
    dir_outputs = os.path.relpath('./psaltlib/Outputs')
    try:
        os.makedirs(dir_inputs)
    except OSError:
        if not os.path.isdir(dir_inputs):
            raise
            print('error: could not create directory "' + dir_inputs + '"')
            sys.exit()
    try:
        os.makedirs(dir_outputs)
    except OSError:
        if not os.path.isdir(dir_outputs):
            raise
            print('error: could not create directory "' + dir_outputs + '"')
            sys.exit()

    # ================================================================================
    # Get inputs, outputs, enumerations, and list of UAVs from Salty file
    # ================================================================================
    # Read in Salty file. Collect Boolean inputs, Boolean outputs, and enumerations.
    salty_text = salty_file.read()
    salty_file.close()
    boolean_inputs = re.findall('input\s+(\w+)\s+:\s+Bool', salty_text)
    boolean_outputs = re.findall('output\s+(\w+)\s+:\s+Bool', salty_text)
    enumerations = re.findall('enum\s+(\w+)\s+=\s+(.+)\n', salty_text)

    # For enumerations used in outputs, get individual elements
    enumeration_elements = []
    for enum in enumerations:
        if re.search('output\s+(\w+)\s+:\s+' + enum[0], salty_text):
            elements = re.split('\s+\|\s+', enum[1])
            enumeration_elements.extend(elements)

    # Prepare to save UAV IDs seen when processing input and output arguments
    uav_ids = []

    # Parse each Boolean input. Assumes format ([a-zA-Z0-9]+)(\_[0-9]+)* where $1 is a function name and
    # subsequent groups of integers are UAV id arguments to pass into the function.
    # Create a dict with the function names as keys. Each key associates a list where the first element is the
    # number of arguments encoded in the input (determined by number of underscores in first usage).
    # Subsequent elements inputs are inputs with the same function name (for error checking).
    # Check that re-uses of function names have the same number of arguments; append full names to key'd list.
    input_dict = dict()
    for salt_input in boolean_inputs:
        sub_strings = re.match('([a-zA-Z0-9]+)(\_[0-9]+)*$', salt_input)
        if sub_strings is None:
            print('Improper format on input ' + salt_input)
            print('Inputs must follow format [a-zA-Z0-9]+(_[0-9]+)*')
            sys.exit()
        uav_args = re.split('_', salt_input)[1:]
        num_uav_args = len(uav_args)
        base_name = sub_strings.groups()[0]
        # if not input_dict.has_key(base_name):
        if base_name not in input_dict:
            input_dict[base_name] = [num_uav_args]
        else:
            if input_dict[base_name][0] != num_uav_args:
                print('Inconsistent number of arguments between ' + salt_input + ' and other inputs(s) '
                      + str(input_dict[base_name][1:]))
                sys.exit()
            else:
                input_dict[base_name].append(salt_input)
        for uav_arg in uav_args:
            if int(uav_arg) not in set(uav_ids):
                uav_ids.append(int(uav_arg))

    # Parse each Boolean output and enumeration element as is done with inputs.
    output_dict = dict()
    for salt_output in boolean_outputs + enumeration_elements:
        sub_strings = re.match('([a-zA-Z0-9]+)(\_[0-9]+)*$', salt_output)
        if sub_strings is None:
            print('Improper format on output ' + salt_output)
            print('Outputs must follow format [a-zA-Z0-9]+(_[0-9]+)*')
            sys.exit()
        uav_args = re.split('_', salt_output)[1:]
        num_uav_args = len(uav_args)
        base_name = sub_strings.groups()[0]

        # if not output_dict.has_key(base_name):
        if base_name not in output_dict:
            output_dict[base_name] = [num_uav_args]
        else:
            if output_dict[base_name][0] != num_uav_args:
                print('Inconsistent number of arguments between ' + salt_output + ' and other output(s) '
                      + str(output_dict[base_name][1:]))
                sys.exit()
            else:
                output_dict[base_name].append(salt_output)

        for uav_arg in uav_args:
            if int(uav_arg) not in set(uav_ids):
                uav_ids.append(int(uav_arg))

    # ================================================================================
    # Create Monitor, Behavior, package, and main script files
    # ================================================================================

    # Create input function files
    for input_name, input_list in input_dict.items():
        file_path = dir_inputs + '/' + input_name + '.py'
        if not os.path.exists(file_path):
            input_file = open(file_path, 'w')
            input_file.write('from ..General import AvState\n')
            input_file.write('import sys\n')
            input_file.write('sys.path.insert(0, \'psaltlib/LMCP/py\')\n')
            input_file.write('from lmcp import LMCPFactory\n\n\n')
            input_file.write('def ' + input_name + '(')
            num_args = input_list[0]
            for ii in range(1, num_args+1):
                input_file.write('av_state_' + str(ii) + ', ')
            input_file.write('socket_send')
            input_file.write('):\n    return\n')
            input_file.close()
            print('Input function ' + file_path + ' created.')
        else:
            print('Input function ' + file_path + ' already exists. Skipping...')

    # Create output function files
    for output_name, output_list in output_dict.items():
        file_path = dir_outputs + '/' + output_name + '.py'
        if not os.path.exists(file_path):
            output_file = open(file_path, 'w')
            output_file.write('from ..General import AvState\n')
            output_file.write('import sys\n')
            output_file.write('sys.path.insert(0, \'psaltlib/LMCP/py\')\n')
            output_file.write('from lmcp import LMCPFactory\n\n\n')
            output_file.write('def ' + output_name + '(')
            num_args = output_list[0]
            for ii in range(1, num_args+1):
                output_file.write('av_state_' + str(ii) + ', ')
            output_file.write('socket_send')
            output_file.write('):\n    return\n')
            output_file.close()
            print('Output function ' + file_path + ' created.')
        else:
            print('Output function ' + file_path + ' already exists. Skipping...')

    # Create __init__.py files in Inputs and Outputs. Set to import all functions from all files.
    input_init_file = open(dir_inputs + '/__init__.py', 'w')
    for function_name in input_dict.keys():
        input_init_file.write('from psaltlib.Inputs.' + function_name + ' import *\n')
    input_init_file.close()
    print('__init__.py in ' + dir_inputs + ' created.')

    output_init_file = open(dir_outputs + '/__init__.py', 'w')
    for function_name in output_dict.keys():
        output_init_file.write('from psaltlib.Outputs.' + function_name + ' import *\n')
    output_init_file.close()
    print('__init__.py in ' + dir_outputs + ' created.')

    # Get name of controller from salty file
    sub_strings = re.findall('controller[ ]+(\w+)[ ]+where', salty_text)
    controller_name = sub_strings[0]

    # Create main file. Open template file and read in text. Close template file.
    sim_file = open(controller_name + '_Simulation.py', 'w')
    template_file = open('./psaltlib/General/template.txt', 'r')
    template_text = template_file.read()
    template_file.close()

    # Replace all instances of @ControllerName@ in template text with controller_name
    template_text = re.sub('@ControllerName@', controller_name, template_text)

    # Replace all instances of @av_ids@ with list of av_ids
    template_text = re.sub('@av_ids@', str(uav_ids), template_text)

    # Replace all instances of @input_variable_names@
    template_text = re.sub('@input_variable_names@', str(boolean_inputs), template_text)

    # Replace all instances of @init_states@ with first inputs of controller
    template_text = re.sub('@init_inputs@', 'list(controller._table[-1].keys()[0])', template_text)

    # Write the file and report it as done
    sim_file.write(template_text)
    sim_file.close()
    print('\nMain simulation file ' + controller_name + '_Simulation.py created.\n')
    print('Done!')


if __name__ == "__main__":
    main(sys.argv[1:])
