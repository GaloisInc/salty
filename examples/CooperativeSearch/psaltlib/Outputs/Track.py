from ..General import AvState
import sys
sys.path.insert(0, 'psaltlib/LMCP/py')
from lmcp import LMCPFactory


def Track(av_state_1, socket_send):

    # If not defined, create storage in the vehicle's internal state for the vehicle's 'behavior'
    if 'behavior' not in av_state_1.internal_state:
        av_state_1.internal_state['behavior'] = 'undefined'

    # If not already tracking, put in an AutomationRequest to track (Task 3)
    if av_state_1.internal_state['behavior'].lower() != 'track':
        av_state_1.internal_state['behavior'] = 'track'
        # Create AutomationRequest to do Track, which is Task 3
        factory = LMCPFactory.LMCPFactory()
        msg_obj = factory.createObjectByName("CMASI", "AutomationRequest")
        msg_obj.EntityList = [av_state_1.ID]
        msg_obj.TaskList = [3]
        header = bytearray(str(msg_obj.FULL_LMCP_TYPE_NAME) + "$lmcp|" + str(msg_obj.FULL_LMCP_TYPE_NAME) + "||0|0$",
                           'ascii')
        smsg = LMCPFactory.packMessage(msg_obj, True)
        socket_send.send(header + smsg)
        # Print status
        print('\nSending ' + msg_obj.FULL_LMCP_TYPE_NAME + ' for Air Vehicle ' + str(av_state_1.ID) +
              ' to perform Track task (TaskID 3)')
        print(msg_obj.toXMLStr(""))
    return
