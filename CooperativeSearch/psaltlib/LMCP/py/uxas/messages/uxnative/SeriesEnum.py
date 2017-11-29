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

import VideoRecord
import StartupComplete
import CreateNewService
import KillService
import IncrementWaypoint
import EntityLocation
import BandwidthTest
import BandwidthReceiveReport
import SubTaskExecution
import SubTaskAssignment
import AutopilotKeepAlive
import OnboardProcessorIsAlive
import EntityJoin
import EntityExit


SERIES_NAME = "UXNATIVE"
#Series Name turned into a long for quick comparisons.
SERIES_NAME_ID = 6149751333668345413
SERIES_VERSION = 3


class SeriesEnum:

    def getName(self, type):
        if(type ==  1): return "VideoRecord"
        if(type ==  2): return "StartupComplete"
        if(type ==  3): return "CreateNewService"
        if(type ==  4): return "KillService"
        if(type ==  5): return "IncrementWaypoint"
        if(type ==  6): return "EntityLocation"
        if(type ==  7): return "BandwidthTest"
        if(type ==  8): return "BandwidthReceiveReport"
        if(type ==  9): return "SubTaskExecution"
        if(type ==  10): return "SubTaskAssignment"
        if(type ==  11): return "AutopilotKeepAlive"
        if(type ==  12): return "OnboardProcessorIsAlive"
        if(type ==  13): return "EntityJoin"
        if(type ==  14): return "EntityExit"


    def getType(self, name):
        if ( name == "VideoRecord"): return 1
        if ( name == "StartupComplete"): return 2
        if ( name == "CreateNewService"): return 3
        if ( name == "KillService"): return 4
        if ( name == "IncrementWaypoint"): return 5
        if ( name == "EntityLocation"): return 6
        if ( name == "BandwidthTest"): return 7
        if ( name == "BandwidthReceiveReport"): return 8
        if ( name == "SubTaskExecution"): return 9
        if ( name == "SubTaskAssignment"): return 10
        if ( name == "AutopilotKeepAlive"): return 11
        if ( name == "OnboardProcessorIsAlive"): return 12
        if ( name == "EntityJoin"): return 13
        if ( name == "EntityExit"): return 14

        return -1

    def getInstance(self, type):
        if(type ==  1): return VideoRecord.VideoRecord()
        if(type ==  2): return StartupComplete.StartupComplete()
        if(type ==  3): return CreateNewService.CreateNewService()
        if(type ==  4): return KillService.KillService()
        if(type ==  5): return IncrementWaypoint.IncrementWaypoint()
        if(type ==  6): return EntityLocation.EntityLocation()
        if(type ==  7): return BandwidthTest.BandwidthTest()
        if(type ==  8): return BandwidthReceiveReport.BandwidthReceiveReport()
        if(type ==  9): return SubTaskExecution.SubTaskExecution()
        if(type ==  10): return SubTaskAssignment.SubTaskAssignment()
        if(type ==  11): return AutopilotKeepAlive.AutopilotKeepAlive()
        if(type ==  12): return OnboardProcessorIsAlive.OnboardProcessorIsAlive()
        if(type ==  13): return EntityJoin.EntityJoin()
        if(type ==  14): return EntityExit.EntityExit()

        return None
