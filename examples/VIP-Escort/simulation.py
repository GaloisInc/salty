#!/usr/bin/python
#import psaltlib.Outputs
#import psaltlib.Inputs
import itertools
import zmq, sys, re, time
sys.path.insert(0, 'lmcp/py')
from lmcp import LMCPFactory
from inspect import getmembers, isfunction
from libs.AvState import *
from Vip import Vip
from time import sleep
from random import choice
sys.path.insert(0, 'ctrl')

class Location():

    def __init__(self, lat, lon):
        self.lat = lat
        self.lon = lon

AVERROR = 'Error: AirVehicleState for ID ' 

context = zmq.Context()
socket_sub = context.socket(zmq.SUB)
socket_sub.connect("tcp://127.0.0.1:5560")
socket_sub.setsockopt(zmq.SUBSCRIBE, 'afrl.cmasi.AirVehicle')
socket_send = context.socket(zmq.PUSH)
socket_send.connect("tcp://127.0.0.1:5561")

def fire_outputs(cmds, states):
    if False: #cmds['vTrack1']:
        track(1)
    else:
        goto(1,int(cmds['uloc1'])+1)
    if False: #cmds['vTrack2']:
        track(2)
    else:
        goto(2,int(cmds['uloc2'])+1)
    goto(4,int(cmds['vloc'])+1)
    enemy_loc = choice([3,4,5])
    goto(3,enemy_loc)
    return enemy_loc

def goto(uav, loc):
    factory = LMCPFactory.LMCPFactory()
    msg_obj = factory.createObjectByName("CMASI", "AutomationRequest")
    msg_obj.EntityList = [uav]
    msg_obj.TaskList = [loc]
    msg_obj.OperatingRegion = 100
    header = str(msg_obj.FULL_LMCP_TYPE_NAME) + "$lmcp|" +\
             str(msg_obj.FULL_LMCP_TYPE_NAME) + "||0|0$"
    smsg = LMCPFactory.packMessage(msg_obj, True)
    socket_send.send(header + smsg)

def track(tracker):
    factory = LMCPFactory.LMCPFactory()
    msg_obj = factory.createObjectByName("CMASI", "AutomationRequest")
    msg_obj.EntityList = [tracker]
    msg_obj.TaskList = [6]
    header = str(msg_obj.FULL_LMCP_TYPE_NAME) + "$lmcp|" +\
             str(msg_obj.FULL_LMCP_TYPE_NAME) + "||0|0$"
    smsg = LMCPFactory.packMessage(msg_obj, True)
    socket_send.send(header + smsg)

def update_avlocs(av_states, avlocs):
    for c,state in enumerate(av_states.values()):
        for i in range(5):
            (t, d) = in_(i, state)
            if t:
                avlocs[c] = i
                break
    return avlocs

def update_inputs(splist, avlocs):
    return {'sp0':splist[0][0], 'sp1':splist[1][0], 'sp2':splist[2][0], 'sp3':splist[3][0], 
            'sp4':splist[4][0], 'vlocs':avlocs[3], 'uloc1s':avlocs[0], 'uloc2s':avlocs[1], 
            'olocs':avlocs[2]}

def in_(sp,state):
    width = 0.0192
    LOCATIONS = [0 for i in range(5)]
    LOCATIONS[0] = Location(45.2871,-121.0122) # bottom left
    LOCATIONS[1] = Location(45.2871,-120.9122) # bottom right
    LOCATIONS[2] = Location(45.3289,-120.9635) # middle
    LOCATIONS[3] = Location(45.3729,-121.0122) # top left
    LOCATIONS[4] = Location(45.3729,-120.9122) # top right
    if state.Latitude > LOCATIONS[sp].lat - width and \
      state.Latitude < LOCATIONS[sp].lat + width and \
      state.Longitude > LOCATIONS[sp].lon - width and \
      state.Longitude < LOCATIONS[sp].lon + width:
          data = (str(state.ID) + " it is at " + str(state.Latitude) + ", " + str(state.Longitude),
                state.Latitude - (LOCATIONS[sp].lat - width),
                (LOCATIONS[sp].lat + width) - state.Latitude,
                (LOCATIONS[sp].lon + width) - state.Longitude,
                state.Longitude - (LOCATIONS[sp].lon - width))
          return (True,data)
    else:
        return (False,0)

def anyIn(sp,uavs):
    for uav in uavs.values():
        if not uav.ID in [3,4]:
            (t, d) = in_(sp[1],uav)
            if t:
                return True
    return False

def isSurv(sp, av_states):
    if sp[0]:
        return True
    else:
        return anyIn(sp,av_states)

def update_surv(splist, av_states):
    splist = [(isSurv(sp,av_states),sp[1]) for sp in splist]
    return splist

def get_next_message(socket_sub, lmcp_factory):
        data = socket_sub.recv()
        address, attributes, msg = data.split('$', 2)
        msg_format, msg_type, msg_group, entityid, serviceid = attributes.split('|', 4)
        msg = msg
        obj = lmcp_factory.getObject(msg)
        if (int(entityid) == 0 and int(
                serviceid) == 0) or obj.FULL_LMCP_TYPE_NAME == "uxas.messages.uxnative.CreateNewService":
            return None
        else:
            return obj

def initialize_av_configurations(av_configurations, av_ids, lmcp_factory, socket_sub):
    while True:
        msg_obj = get_next_message(socket_sub, lmcp_factory)
        if msg_obj and msg_obj.FULL_LMCP_TYPE_NAME == 'afrl.cmasi.AirVehicleConfiguration':
            if msg_obj.get_ID() in av_ids:
                if not av_configurations.has_key(msg_obj.get_ID()):
                    print('Recording AirVehicleConfiguration with ID ' + str(msg_obj.get_ID()))
                else:
                    print('Warning: Recording duplicate AirVehicleConfiguration with ID ' + str(msg_obj.get_ID()))
                av_configurations[msg_obj.get_ID()] = msg_obj
            else:
                print('Warning: Ignoring AirVehicleConfiguration with ID ' + str(msg_obj.get_ID()) + \
                      ' not used in salt file')
        if msg_obj and msg_obj.FULL_LMCP_TYPE_NAME == 'afrl.cmasi.AirVehicleState':
            print('Saw first AirVehicleState with ID ' + str(msg_obj.get_ID()))
            if len(av_ids.difference(set(av_configurations.keys()))) > 0:
                print('Error: Missing AirVehicleConfigurations for ID(s): ' + ", ".join(str(e) for e in av_ids))
                sys.exit()
            break
    return msg_obj

def initialize_av_states(av_configurations, av_states, msg_obj, lmcp_factory, socket_sub):
    av_initializations = av_configurations.keys()
    while len(av_initializations) > 0:
        if msg_obj and msg_obj.FULL_LMCP_TYPE_NAME == 'afrl.cmasi.AirVehicleState':
            if not av_configurations.has_key(msg_obj.get_ID()):
                print('Warning: Ignoring unknown AirVehicleState ' + str(msg_obj.get_ID()))
            elif av_initializations.count(msg_obj.get_ID()) == 1:
                av_states[msg_obj.get_ID()] = AvState(av_configurations[msg_obj.get_ID()], 
                        msg_obj)
                av_initializations.remove(msg_obj.get_ID())
            else:
                print(av_error + str(msg_obj.get_ID()) + ' seen twice in one cycle.')
        msg_obj = get_next_message(socket_sub, lmcp_factory)
    return msg_obj

def update_av_states(av_states, msg_obj, lmcp_factory, socket_sub):
    av_initializations = av_states.keys()
    while len(av_initializations) > 0:
        if msg_obj and msg_obj.FULL_LMCP_TYPE_NAME == 'afrl.cmasi.AirVehicleState':
            if not av_states.has_key(msg_obj.get_ID()):
                print('Warning: Ignoring unknown AirVehicleState ' + str(msg_obj.get_ID()))
            elif av_initializations.count(msg_obj.get_ID()) == 1:
                av_states[msg_obj.get_ID()].update_state(msg_obj)
                av_initializations.remove(msg_obj.get_ID())
            else:
                print(AVERROR + str(msg_obj.get_ID()) + ' seen twice in one cycle.')
        msg_obj = get_next_message(socket_sub, lmcp_factory)
    return (msg_obj, av_states)

def main():
    controller = Vip()
    print("Initiated: Reactive Controller")
    splist = [(False,i) for i in range(5)]
    lmcp_factory = LMCPFactory.LMCPFactory()
    av_configurations = dict()
    av_states = dict()
    av_ids = set([1, 2, 3, 4])
    msg_obj = initialize_av_configurations(av_configurations, av_ids, 
            lmcp_factory, socket_sub)
    time.sleep(1.0)
    msg_obj = initialize_av_states(av_configurations, av_states, msg_obj, 
            lmcp_factory, socket_sub)
    avlocs = [0 for i in range(4)]
    while True:
        avlocs = update_avlocs(av_states, avlocs)
        splist = update_surv(splist,av_states)
        input_states = update_inputs(splist, avlocs)
        ctrl_input = rm_noninputs(input_states.copy())
        output_state = controller.move(**ctrl_input)
        enemy_loc = fire_outputs(output_state, av_states)
        while (input_states['vlocs'] != output_state['vloc'] or
                input_states['uloc1s'] != output_state['uloc1'] or
                input_states['uloc2s'] != output_state['uloc2'] or
                input_states['olocs'] != enemy_loc-1):
            
            avlocs = update_avlocs(av_states, avlocs)
            splist = update_surv(splist,av_states)
            input_states = update_inputs(splist, avlocs)
            (msg_obj, av_states) = update_av_states(av_states, msg_obj, lmcp_factory, socket_sub)

def rm_noninputs(inp):
    del inp['vlocs']
    del inp['uloc1s']
    del inp['uloc2s']
    return inp

if __name__ == '__main__':
    main()
