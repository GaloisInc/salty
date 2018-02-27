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

import AssignmentCoordinatorTask
import RendezvousTask
import PlanningState
import AssignmentCoordination
import CoordinatedAutomationRequest
import TaskAutomationRequest
import TaskAutomationResponse
import UniqueAutomationRequest
import UniqueAutomationResponse
import SensorFootprintRequests
import FootprintRequest
import SensorFootprint
import SensorFootprintResponse
import TaskImplementationRequest
import TaskImplementationResponse
import AssignmentCostMatrix
import TaskOptionCost
import TaskAssignment
import TaskAssignmentSummary
import TaskOption
import TaskPlanOptions
import TaskPause
import TaskResume
import TaskProgress
import TaskProgressRequest
import TaskInitialized
import TaskActive
import TaskComplete
import CancelTask


SERIES_NAME = "UXTASK"
#Series Name turned into a long for quick comparisons.
SERIES_NAME_ID = 6149757930721443840
SERIES_VERSION = 7


class SeriesEnum:

    def getName(self, type):
        if(type ==  1): return "AssignmentCoordinatorTask"
        if(type ==  2): return "RendezvousTask"
        if(type ==  3): return "PlanningState"
        if(type ==  4): return "AssignmentCoordination"
        if(type ==  5): return "CoordinatedAutomationRequest"
        if(type ==  6): return "TaskAutomationRequest"
        if(type ==  7): return "TaskAutomationResponse"
        if(type ==  8): return "UniqueAutomationRequest"
        if(type ==  9): return "UniqueAutomationResponse"
        if(type ==  10): return "SensorFootprintRequests"
        if(type ==  11): return "FootprintRequest"
        if(type ==  12): return "SensorFootprint"
        if(type ==  13): return "SensorFootprintResponse"
        if(type ==  14): return "TaskImplementationRequest"
        if(type ==  15): return "TaskImplementationResponse"
        if(type ==  16): return "AssignmentCostMatrix"
        if(type ==  17): return "TaskOptionCost"
        if(type ==  18): return "TaskAssignment"
        if(type ==  19): return "TaskAssignmentSummary"
        if(type ==  20): return "TaskOption"
        if(type ==  21): return "TaskPlanOptions"
        if(type ==  22): return "TaskPause"
        if(type ==  23): return "TaskResume"
        if(type ==  24): return "TaskProgress"
        if(type ==  25): return "TaskProgressRequest"
        if(type ==  26): return "TaskInitialized"
        if(type ==  27): return "TaskActive"
        if(type ==  28): return "TaskComplete"
        if(type ==  29): return "CancelTask"


    def getType(self, name):
        if ( name == "AssignmentCoordinatorTask"): return 1
        if ( name == "RendezvousTask"): return 2
        if ( name == "PlanningState"): return 3
        if ( name == "AssignmentCoordination"): return 4
        if ( name == "CoordinatedAutomationRequest"): return 5
        if ( name == "TaskAutomationRequest"): return 6
        if ( name == "TaskAutomationResponse"): return 7
        if ( name == "UniqueAutomationRequest"): return 8
        if ( name == "UniqueAutomationResponse"): return 9
        if ( name == "SensorFootprintRequests"): return 10
        if ( name == "FootprintRequest"): return 11
        if ( name == "SensorFootprint"): return 12
        if ( name == "SensorFootprintResponse"): return 13
        if ( name == "TaskImplementationRequest"): return 14
        if ( name == "TaskImplementationResponse"): return 15
        if ( name == "AssignmentCostMatrix"): return 16
        if ( name == "TaskOptionCost"): return 17
        if ( name == "TaskAssignment"): return 18
        if ( name == "TaskAssignmentSummary"): return 19
        if ( name == "TaskOption"): return 20
        if ( name == "TaskPlanOptions"): return 21
        if ( name == "TaskPause"): return 22
        if ( name == "TaskResume"): return 23
        if ( name == "TaskProgress"): return 24
        if ( name == "TaskProgressRequest"): return 25
        if ( name == "TaskInitialized"): return 26
        if ( name == "TaskActive"): return 27
        if ( name == "TaskComplete"): return 28
        if ( name == "CancelTask"): return 29

        return -1

    def getInstance(self, type):
        if(type ==  1): return AssignmentCoordinatorTask.AssignmentCoordinatorTask()
        if(type ==  2): return RendezvousTask.RendezvousTask()
        if(type ==  3): return PlanningState.PlanningState()
        if(type ==  4): return AssignmentCoordination.AssignmentCoordination()
        if(type ==  5): return CoordinatedAutomationRequest.CoordinatedAutomationRequest()
        if(type ==  6): return TaskAutomationRequest.TaskAutomationRequest()
        if(type ==  7): return TaskAutomationResponse.TaskAutomationResponse()
        if(type ==  8): return UniqueAutomationRequest.UniqueAutomationRequest()
        if(type ==  9): return UniqueAutomationResponse.UniqueAutomationResponse()
        if(type ==  10): return SensorFootprintRequests.SensorFootprintRequests()
        if(type ==  11): return FootprintRequest.FootprintRequest()
        if(type ==  12): return SensorFootprint.SensorFootprint()
        if(type ==  13): return SensorFootprintResponse.SensorFootprintResponse()
        if(type ==  14): return TaskImplementationRequest.TaskImplementationRequest()
        if(type ==  15): return TaskImplementationResponse.TaskImplementationResponse()
        if(type ==  16): return AssignmentCostMatrix.AssignmentCostMatrix()
        if(type ==  17): return TaskOptionCost.TaskOptionCost()
        if(type ==  18): return TaskAssignment.TaskAssignment()
        if(type ==  19): return TaskAssignmentSummary.TaskAssignmentSummary()
        if(type ==  20): return TaskOption.TaskOption()
        if(type ==  21): return TaskPlanOptions.TaskPlanOptions()
        if(type ==  22): return TaskPause.TaskPause()
        if(type ==  23): return TaskResume.TaskResume()
        if(type ==  24): return TaskProgress.TaskProgress()
        if(type ==  25): return TaskProgressRequest.TaskProgressRequest()
        if(type ==  26): return TaskInitialized.TaskInitialized()
        if(type ==  27): return TaskActive.TaskActive()
        if(type ==  28): return TaskComplete.TaskComplete()
        if(type ==  29): return CancelTask.CancelTask()

        return None
