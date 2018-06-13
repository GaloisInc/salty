from ..General import AvState
import sys
sys.path.insert(0, 'psaltlib/LMCP/py')
from lmcp import LMCPFactory


def Return(av_state_1, socket_send):

    # If not defined, create storage in the vehicle's internal state for the vehicle's 'behavior'
    if 'behavior' not in av_state_1.internal_state:
        av_state_1.internal_state['behavior'] = 'undefined'

    # If not already returning, put in an AutomationRequest to return to the recall point (Task 4)
    if av_state_1.internal_state['behavior'].lower() != 'return':
        av_state_1.internal_state['behavior'] = 'return'
        # Create AutomationRequest to do Return, which is Task 4
        factory = LMCPFactory.LMCPFactory()
        msg_obj = factory.createObjectByName("CMASI", "AutomationRequest")
        msg_obj.EntityList = [av_state_1.ID]
        msg_obj.TaskList = [4]
        header = bytearray(str(msg_obj.FULL_LMCP_TYPE_NAME) + "$lmcp|" + str(msg_obj.FULL_LMCP_TYPE_NAME) + "||0|0$",
                           'ascii')
        smsg = LMCPFactory.packMessage(msg_obj, True)
        socket_send.send(header + smsg)
        # Print status
        print('\nSending ' + msg_obj.FULL_LMCP_TYPE_NAME + ' for Air Vehicle ' + str(av_state_1.ID) +
              ' to perform Return task (TaskID 4)')
        print(msg_obj.toXMLStr(""))
    return
