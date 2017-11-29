from lmcp.LMCPObject import *

## ===============================================================================
## Authors: AFRL/RQQA
## Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
## 
## Copyright (c) 2017 Government of the United State of America, as represented by
## the Secretary of the Air Force.  No copyright is claimed in the United States under
## Title 17, U.S. Code.  All Other Rights Reserved.
## ===============================================================================

## This file was auto-created by LmcpGen. Modifications will be overwritten.

import EntityPerception
import TrackEntityAction
import TrackEntityTask


SERIES_NAME = "PERCEIVE"
#Series Name turned into a long for quick comparisons.
SERIES_NAME_ID = 5784119745305990725
SERIES_VERSION = 1


class SeriesEnum:

    def getName(self, type):
        if(type ==  1): return "EntityPerception"
        if(type ==  2): return "TrackEntityAction"
        if(type ==  3): return "TrackEntityTask"


    def getType(self, name):
        if ( name == "EntityPerception"): return 1
        if ( name == "TrackEntityAction"): return 2
        if ( name == "TrackEntityTask"): return 3

        return -1

    def getInstance(self, type):
        if(type ==  1): return EntityPerception.EntityPerception()
        if(type ==  2): return TrackEntityAction.TrackEntityAction()
        if(type ==  3): return TrackEntityTask.TrackEntityTask()

        return None
