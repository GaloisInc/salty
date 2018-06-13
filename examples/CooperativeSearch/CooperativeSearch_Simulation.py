#!/usr/bin/python
import sys
sys.path.insert(0, 'psaltlib/LMCP/py')
from CooperativeSearch import CooperativeSearch
import psaltlib.Outputs
import psaltlib.Inputs
from lmcp import LMCPFactory
import zmq, re, time
from inspect import getmembers, isfunction
from psaltlib.General.AvState import *


def main():

    # Prepare LMCP factory
    lmcp_factory = LMCPFactory.LMCPFactory()

    # Set up send and receive sockets to UxAS (must match ports in UxAS's cfg_<scenarioName>.xml file).
    context = zmq.Context()
    socket_sub = context.socket(zmq.SUB)
    socket_sub.connect("tcp://127.0.0.1:5560")
    socket_sub.setsockopt(zmq.SUBSCRIBE, b'afrl.cmasi.AirVehicle')
    socket_send = context.socket(zmq.PUSH)
    socket_send.connect("tcp://127.0.0.1:5561")

    # Define list of output variable names and AirVehicle IDs referenced in Salty file.
    av_ids = set([1, 2, 3])
    input_variable_names = ['recalled_1', 'recalled_2', 'inRange_1_3', 'inRange_2_3']

    # Set up dictionaries to store AirVehicleConfigurations and AirVehicleStates key'd by ID.
    av_configurations = dict()
    av_states = dict()

    # Record AirVehicleConfigurations. Stop once an AirVehicleState is seen.
    # Ensure all expected UAVs referenced in Salty file are initialized. Warn about duplicates.
    msg_obj = initialize_av_configurations(av_configurations, av_ids, lmcp_factory, socket_sub)

    # Pause to give time for UxAS to define tasks
    time.sleep(3.0)

    # Construct the Slugs controller.
    controller = CooperativeSearch()

    # Initialize dictionary of AvStates.
    msg_obj = initialize_av_states(av_configurations, av_states, msg_obj, lmcp_factory, socket_sub)

    # ========================= Main Loop =========================
    # Update input variables, move the controller, fire outputs,
    # then update AirVehicleStates.
    while True:
        input_states = update_inputs(av_states, socket_send, *input_variable_names)
        output_state = controller.move(*input_states)
        fire_outputs(output_state, av_states, socket_send)
        msg_obj = update_av_states(av_states, msg_obj, lmcp_factory, socket_sub)


def get_next_message(socket_sub, lmcp_factory):
        data = socket_sub.recv()
        # messages are single part with a header followed by serialized LMCP header format:
        # [address]$[format]|[type]|[group]|[entity]|[service]$
        address, attributes, msg = data.split(bytearray('$','ascii'), 2)
        msg_format, msg_type, msg_group, entityid, serviceid = attributes.split(bytearray('|','ascii'), 4)
        obj = lmcp_factory.getObject(msg)

        # sending as entityid{0} and serviceid{0}, so check for loopback
        # CreateServiceMessage currently has xml parsing problems, so remove
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
                if msg_obj.get_ID() not in av_configurations:
                    print('Recording AirVehicleConfiguration with ID ' + str(msg_obj.get_ID()))
                else:
                    print('Warning: Recording duplicate AirVehicleConfiguration with ID ' + str(msg_obj.get_ID()))
                av_configurations[msg_obj.get_ID()] = msg_obj
            else:
                print('Warning: Ignoring AirVehicleConfiguration with ID ' + str(msg_obj.get_ID()) +
                      ' not used in salt file')
        if msg_obj and msg_obj.FULL_LMCP_TYPE_NAME == 'afrl.cmasi.AirVehicleState':
            print('Saw first AirVehicleState with ID ' + str(msg_obj.get_ID()))
            if len(av_ids.difference(set(av_configurations.keys()))) > 0:
                print('Error: Missing AirVehicleConfigurations for ID(s): ' + ", ".join(str(e) for e in av_ids))
                sys.exit()
            break
    return msg_obj


def initialize_av_states(av_configurations, av_states, msg_obj, lmcp_factory, socket_sub):
    av_initializations = list(av_configurations.keys())
    while len(av_initializations) > 0:
        if msg_obj and msg_obj.FULL_LMCP_TYPE_NAME == 'afrl.cmasi.AirVehicleState':
            if msg_obj.get_ID() not in av_configurations:
                print('Warning: Ignoring unknown AirVehicleState ' + str(msg_obj.get_ID()))
            elif av_initializations.count(msg_obj.get_ID()) == 1:
                av_states[msg_obj.get_ID()] = AvState(av_configurations[msg_obj.get_ID()], msg_obj)
                av_initializations.remove(msg_obj.get_ID())
            #else:
                #print('Error: AirVehicleState for ID ' + str(msg_obj.get_ID()) + ' seen twice in one cycle.')
                #sys.exit()
        msg_obj = get_next_message(socket_sub, lmcp_factory)
    return msg_obj


def update_av_states(av_states, msg_obj, lmcp_factory, socket_sub):
    av_initializations = list(av_states.keys())
    while len(av_initializations) > 0:
        if msg_obj and msg_obj.FULL_LMCP_TYPE_NAME == 'afrl.cmasi.AirVehicleState':
            if msg_obj.get_ID() not in av_states:
                print('Warning: Ignoring unknown AirVehicleState ' + str(msg_obj.get_ID()))
            elif av_initializations.count(msg_obj.get_ID()) == 1:
                av_states[msg_obj.get_ID()].update_state(msg_obj)
                av_initializations.remove(msg_obj.get_ID())
            #else:
                #print('Error: AirVehicleState for ID ' + str(msg_obj.get_ID()) + ' seen twice in one cycle.')
                #sys.exit()
        msg_obj = get_next_message(socket_sub, lmcp_factory)
    return msg_obj


def fire_outputs(output_state, av_states, socket_send):
    # output_state is a dictionary with output variable names as keys. Each key'd value is either
    # a Boolean (if True, then execute the function with AvState arguments encoded by the key), or
    # an enumeration element (then execute the function with AvState arguments encoded by the key'd value).
    for key in output_state.keys():
        # Determine whether key'd value is an enumeration or a Boolean in order to retrieve the name of the term.
        if type(output_state[key]) is not type(True):
            term = output_state[key].name
        else:
            term = key
        # Get the function name
        term_elements = re.split('_', term)
        function_name = term_elements[0]
        # Set up the arguments to pass to the function, i.e. AirVehicle IDs in the term and the send_socket
        argument_av_ids = term_elements[1:]
        args = []
        for av_id in argument_av_ids:
            args.append(av_states[int(av_id)])
        args.append(socket_send)
        # Slugs generally changes the case of the term to all caps for enumerations, so find some version
        # of function_name in psaltlib.Outputs. Error if none found or multiple found.
        func = [o for o in getmembers(psaltlib.Outputs) if isfunction(o[1]) and
                    o[0].lower() == function_name.lower()]
        if len(func) > 1:
            print('Error: Multiple functions with case insensitive name ' + function_name + 'found')
            sys.exit()
        if len(func) == 0:
            print('Error: No functions with case insensitive name ' + function_name + 'found')
            sys.exit()
        # Call the function if it's not a Boolean currently set to False
        if output_state[key] is not False:
            func[0][1](*args)
    return


def update_inputs(av_states, socket_send, *input_variable_names):
    # From each input variable name, extract the function name, ids of AvStates to be passed as arguments,
    # and the corresponding function from psaltlib.Inputs. Call the function with the id-key'd AvStates as
    # arguments. Aggregate results in order in a list.
    input_state = []
    for term in input_variable_names:
        term_elements = re.split('_', term)
        function_name = term_elements[0]
        av_id_args = term_elements[1:]
        args = []
        for av_id in av_id_args:
            args.append(av_states[int(av_id)])
        args.append(socket_send)
        func = [o for o in getmembers(psaltlib.Inputs) if isfunction(o[1]) and
                    o[0] == function_name]
        input_state.append(func[0][1](*args))
    return input_state


if __name__ == '__main__':
    main()