#! /usr/bin/python

## ===============================================================================
## Authors: AFRL/RQQA
## Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
## 
## Copyright (c) 2017 Government of the United State of America, as represented by
## the Secretary of the Air Force.  No copyright is claimed in the United States under
## Title 17, U.S. Code.  All Other Rights Reserved.
## ===============================================================================

## This file was auto-created by LmcpGen. Modifications will be overwritten.

class SensedVehicleType:

    Treaded = 1
    Wheeled = 2
    Hovercraft = 3



def get_SensedVehicleType_str(str):
    """
    Returns a numerical value from a string
    """
    if str == "Treaded": return SensedVehicleType.Treaded
    if str == "Wheeled": return SensedVehicleType.Wheeled
    if str == "Hovercraft": return SensedVehicleType.Hovercraft


def get_SensedVehicleType_int(val):
    """
    Returns a string representation from an int
    """
    if val == SensedVehicleType.Treaded: return "Treaded"
    if val == SensedVehicleType.Wheeled: return "Wheeled"
    if val == SensedVehicleType.Hovercraft: return "Hovercraft"
    return SensedVehicleType.Treaded


