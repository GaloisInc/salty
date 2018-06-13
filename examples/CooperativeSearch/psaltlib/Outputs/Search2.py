from ..General import AvState
import sys
sys.path.insert(0, 'psaltlib/LMCP/py')
from lmcp import LMCPFactory


def Search2(av_state_1, socket_send):

    # If not defined, create storage in the vehicle's internal state for the vehicle's 'behavior'
    if 'behavior' not in av_state_1.internal_state:
        av_state_1.internal_state['behavior'] = 'undefined'

    # If not already searching area 2, put in an AutomationRequest for a Search task of area 2 (Task 2)
    if av_state_1.internal_state['behavior'].lower() != 'search2':
        av_state_1.internal_state['behavior'] = 'search2'
        # Create AutomationRequest to do Search2, which is Task 2
        factory = LMCPFactory.LMCPFactory()
        msg_obj = factory.createObjectByName("CMASI", "AutomationRequest")
        msg_obj.EntityList = [av_state_1.ID]
        msg_obj.TaskList = [2]
        msg_obj.OperatingRegion = 100
        header = bytearray(str(msg_obj.FULL_LMCP_TYPE_NAME) + "$lmcp|" + str(msg_obj.FULL_LMCP_TYPE_NAME) + "||0|0$",
                           'ascii')
        smsg = LMCPFactory.packMessage(msg_obj, True)
        socket_send.send(header + smsg)
        # Print status
        print('\nSending ' + msg_obj.FULL_LMCP_TYPE_NAME + ' for Air Vehicle ' + str(av_state_1.ID) +
              ' to perform Search2 task (TaskID 2)')
        print(msg_obj.toXMLStr(""))
    return
