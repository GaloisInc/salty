from ..General import AvState
import sys
sys.path.insert(0, 'psaltlib/LMCP/py')
from lmcp import LMCPFactory


def inRange(av_state_1, av_state_2, socket_send):

    # If not defined, create storage in the vehicle's internal state for the truth value of 'inRange'
    # Define key for this vehicle's inRange status
    if 'inRange' not in av_state_1.internal_state:
        av_state_1.internal_state['inRange'] = 'undefined'

    # Check whether inRange. If status changes, print message
    if av_state_1.distance(av_state_2) < 1000:
        if av_state_1.internal_state['inRange'] is False:
            av_state_1.internal_state['inRange'] = True
            print('inRange' + str(av_state_1.ID) + ': True')
        return True
    else:
        if av_state_1.internal_state['inRange'] is True:
            av_state_1.internal_state['inRange'] = False
            print('inRange' + str(av_state_1.ID) + ': False')
        return False
