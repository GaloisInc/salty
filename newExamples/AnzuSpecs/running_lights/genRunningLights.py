#!/usr/bin/python

# Python script to generate GR(1) specification for a series of N LEDs that sequentially light up. Spec requires LED 0
# to eventually light up if button 1 is pressed and LED N to eventually light up if button 2 is pressed. Generates
# file [prefix]running_lights_<num_lights>.salt.
#
# Usage: ./genRunningLights.py <numLights> [fileName] [controllerName]
#
# Default fileName is running_lights<numLights>.salt
# Default controllerName is RunningLights<numLights>

import getopt
import sys
import re


def main(argv):
    # Process input arguments
    try:
        opts, args = getopt.getopt(argv, "h", [])
    except getopt.GetoptError:
        print('usage: python3 genRunningLights.py <num_lights> [file_name] [controller_name]')
        sys.exit()
    if len(args) < 1 or len(args) > 3:
        print('usage: python3 genRunningLights.py <num_lights> [file_name] [controller_name]')
        sys.exit()
    num_lights = int(args[0])
    if num_lights < 2:
        print('<num_lights> must be greater than 1')
        sys.exit()

    file_name = args[1] if len(args) >= 2 else "running_lights_" + str(num_lights) + ".salt"
    controller_name = args[2] if len(args) >= 3 else "RunningLights" + str(num_lights)

    # Try to open file for writing
    try:
        output_file = open(file_name, "w")
    except Exception as e:
        print('error: could not open file ' + output_filename)
        sys.exit()

    output_file.write("controller " + controller_name + " where\n\n" +
                      "input button1 : Bool = False\n" +
                      "input button2 : Bool = False\n\n" +
                      "output state1 : Bool = False\n" +
                      "output state2 : Bool = False\n" +
                      "enum LightNumber = " + light_enumeration_string(num_lights) + "\n" +
                      "output light : LightNumber = L0\n\n"+
                      "env_liveness\n" +
                      "  True\n\n" +
                      "sys_trans\n" +
                      "  !state1 /\ (!button1 \/ light == L0) -> !state1'\n" +
                      "  !state1 /\ ( button1 /\ !light == L0) ->  state1'\n" +
                      "  state1  /\ !light == L0 ->  state1'\n" +
                      "  state1  /\  light == L0 -> !state1'\n\n" +
                      "  !state2 /\ (!button2 \/  light == L" + str(num_lights-1) + ") -> !state2'\n" +
                      "  !state2 /\ ( button2 /\ !light == L" + str(num_lights-1) + ") ->  state2'\n" +
                      "  state2  /\ !light == L" + str(num_lights-1) + " ->  state2'\n" +
                      "  state2  /\  light == L" + str(num_lights-1) + " -> !state2'\n\n" +
                      light_transition_string(num_lights, 2) + "\n" +
                      "  light == L0 /\ !state1' /\ !state2' -> light' == L0\n" +
                      "  light == L" + str(num_lights-1) + " /\ !state1' /\ !state2' -> light' == L" + str(num_lights-1) + "\n" +
                      "  light == L0 /\ !state1  /\ !state2  -> light' == L0\n" +
                      "  light == L" + str(num_lights-1) + " /\ !state1  /\ !state2  -> light' == L" + str(num_lights-1) + "\n\n" +
                      "sys_liveness\n" +
                      "  !state1\n\n" +
                      "sys_liveness\n" +
                      "  !state2\n")


def light_enumeration_string(num_lights):
    s = ""
    for i in range(0, num_lights-1):
        s += "L" + str(i) + " | "
    s += "L" + str(num_lights-1)
    return s


def light_transition_string(num_lights, indent):
    w = " "*indent
    s = w + "!light == L0 /\ !light == L1 -> !light' == L0\n"
    for n in range(1, num_lights-1):
        s += w + "!light == L" + str(n-1) + " /\ !light == L" + str(n+1) + " -> !light' == L" + str(n) +"\n"
    s += w + "!light == L" + str(num_lights-2) + " /\ !light == L" + str(num_lights-1) + \
             " -> !light' == L" + str(num_lights-1) + "\n"
    return s


if __name__ == "__main__":
   main(sys.argv[1:])
