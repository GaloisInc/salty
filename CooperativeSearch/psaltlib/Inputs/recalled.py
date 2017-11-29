from ..General import AvState
import sys
sys.path.insert(0, 'psaltlib/LMCP/py')
from lmcp import LMCPFactory

# User defined imports
import math, LocalCoords
import re
import select
import sys


def recalled(av_state_1, socket_send):
    # On the first loop, tell the user they can enter the vehicle's ID to recall the vehicle
    if not av_state_1.internal_state.has_key('is_first_recall_loop'):
        print '\nEnter ' + str(av_state_1.ID) + ' in console at any time to recall AirVehicle ' + \
              str(av_state_1.ID) + '\n'
        av_state_1.internal_state['is_first_recall_loop'] = True

    # If not defined, create storage in the vehicle's internal state for the vehicle's 'behavior'
    if not av_state_1.internal_state.has_key('behavior'):
        av_state_1.internal_state['behavior'] = 'undefined'

    # If not defined, create storage in the vehicle's internal state for the truth value of 'recalled'
    if not av_state_1.internal_state.has_key('is_recalled'):
        av_state_1.internal_state['is_recalled'] = 'undefined'

    # If not defined, create storage in all vehicle's shared state for the last keyboard input
    # and what vehicles have seen it
    if not av_state_1.shared_state.has_key('keyboard_input'):
        av_state_1.shared_state['keyboard_input'] = None
        av_state_1.shared_state['avs_that_have_seen_keyboard_input'] = set()

    # If the vehicle is not currently returning due to a previous recall, check for recall condition
    if av_state_1.internal_state['behavior'].lower() != 'return':
        # Check whether to get a new input from the keyboard or use the existing one
        if av_state_1.shared_state['keyboard_input'] is None \
                or av_state_1.ID in av_state_1.shared_state['avs_that_have_seen_keyboard_input']:
            line = get_line_from_keyboard()
            av_state_1.shared_state['keyboard_input'] = line
            av_state_1.shared_state['avs_that_have_seen_keyboard_input'] = {av_state_1.ID}
        else:
            line = av_state_1.shared_state['keyboard_input']
            av_state_1.shared_state['avs_that_have_seen_keyboard_input'].add(av_state_1.ID)
        # Check whether the keyboard input matches this vehicle's ID, clear it out of it does,
        # and change recalled status if necessary
        if line is not None and re.match(str(av_state_1.ID) + '[^0-9]*', line) is not None:
            av_state_1.shared_state['keyboard_input'] = None
            av_state_1.shared_state['avs_that_have_seen_keyboard_input'] = set()
            if av_state_1.internal_state['is_recalled'] is not True:
                av_state_1.internal_state['is_recalled'] = True
                print 'recalled_' + str(av_state_1.ID) + ': True'
            return True
        else:
            if av_state_1.internal_state['is_recalled'] is not False:
                av_state_1.internal_state['is_recalled'] = False
                print 'recalled_' + str(av_state_1.ID) + ': False'
            return False

    # If the vehicle is currently recalled, check to see whether it's reached the recall point (within a threshold),
    # and change recalled status if necessary
    else:
        recall_latitude = 30.43258611
        recall_longitude = -87.17408333
        av_xy = LocalCoords.LatLong_degToNorthEast_m(av_state_1.Latitude, av_state_1.Longitude)
        refuel_xy = LocalCoords.LatLong_degToNorthEast_m(recall_latitude, recall_longitude)
        dist = math.sqrt((av_xy[0] - refuel_xy[0]) ** 2 + (av_xy[1] - refuel_xy[1]) ** 2)
        if dist < 500:
            if av_state_1.internal_state['is_recalled'] is not False:
                av_state_1.internal_state['is_recalled'] = False
                print 'recalled_' + str(av_state_1.ID) + ': False'
            return False
        else:
            if av_state_1.internal_state['is_recalled'] is not True:
                av_state_1.internal_state['is_recalled'] = True
                print 'recalled_' + str(av_state_1.ID) + ': True'
            return True


            # User defined non-blocking function to get a line of text from the keyboard
            # Probably only works in *nix systems.


def get_line_from_keyboard():
    i, o, e = select.select([sys.stdin], [], [], 0.001)
    for s in i:
        if s == sys.stdin:
            line = sys.stdin.readline()
            return line
    return None
