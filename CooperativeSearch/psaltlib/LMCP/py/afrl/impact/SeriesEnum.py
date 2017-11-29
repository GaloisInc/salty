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

import PowerConfiguration
import RadioConfiguration
import RadioTowerConfiguration
import RadioState
import RadioTowerState
import ImpactPayloadConfiguration
import DeployImpactPayload
import PowerPlantState
import BatchRoutePlanRequest
import BatchRoutePlanResponse
import TaskTimingPair
import BatchSummaryRequest
import BatchSummaryResponse
import TaskSummary
import VehicleSummary
import ImpactHeartbeat
import ImpactComponentJoin
import ImpactComponentLeave
import SpeedAltPair
import ImpactAutomationRequest
import ImpactAutomationResponse
import PointOfInterest
import LineOfInterest
import AreaOfInterest
import ImpactPointSearchTask
import PatternSearchTask
import AngledAreaSearchTask
import ImpactLineSearchTask
import WatchTask
import MultiVehicleWatchTask
import CommRelayTask
import CordonTask
import BlockadeTask
import EscortTask
import ConfigurationRequest
import WaterReport
import WaterZone


SERIES_NAME = "IMPACT"
#Series Name turned into a long for quick comparisons.
SERIES_NAME_ID = 5281966179208134656
SERIES_VERSION = 13


class SeriesEnum:

    def getName(self, type):
        if(type ==  1): return "PowerConfiguration"
        if(type ==  2): return "RadioConfiguration"
        if(type ==  3): return "RadioTowerConfiguration"
        if(type ==  4): return "RadioState"
        if(type ==  5): return "RadioTowerState"
        if(type ==  6): return "ImpactPayloadConfiguration"
        if(type ==  7): return "DeployImpactPayload"
        if(type ==  8): return "PowerPlantState"
        if(type ==  9): return "BatchRoutePlanRequest"
        if(type ==  10): return "BatchRoutePlanResponse"
        if(type ==  11): return "TaskTimingPair"
        if(type ==  12): return "BatchSummaryRequest"
        if(type ==  13): return "BatchSummaryResponse"
        if(type ==  14): return "TaskSummary"
        if(type ==  15): return "VehicleSummary"
        if(type ==  16): return "ImpactHeartbeat"
        if(type ==  17): return "ImpactComponentJoin"
        if(type ==  18): return "ImpactComponentLeave"
        if(type ==  19): return "SpeedAltPair"
        if(type ==  20): return "ImpactAutomationRequest"
        if(type ==  21): return "ImpactAutomationResponse"
        if(type ==  22): return "PointOfInterest"
        if(type ==  23): return "LineOfInterest"
        if(type ==  24): return "AreaOfInterest"
        if(type ==  25): return "ImpactPointSearchTask"
        if(type ==  26): return "PatternSearchTask"
        if(type ==  27): return "AngledAreaSearchTask"
        if(type ==  28): return "ImpactLineSearchTask"
        if(type ==  29): return "WatchTask"
        if(type ==  30): return "MultiVehicleWatchTask"
        if(type ==  31): return "CommRelayTask"
        if(type ==  32): return "CordonTask"
        if(type ==  33): return "BlockadeTask"
        if(type ==  34): return "EscortTask"
        if(type ==  35): return "ConfigurationRequest"
        if(type ==  36): return "WaterReport"
        if(type ==  37): return "WaterZone"


    def getType(self, name):
        if ( name == "PowerConfiguration"): return 1
        if ( name == "RadioConfiguration"): return 2
        if ( name == "RadioTowerConfiguration"): return 3
        if ( name == "RadioState"): return 4
        if ( name == "RadioTowerState"): return 5
        if ( name == "ImpactPayloadConfiguration"): return 6
        if ( name == "DeployImpactPayload"): return 7
        if ( name == "PowerPlantState"): return 8
        if ( name == "BatchRoutePlanRequest"): return 9
        if ( name == "BatchRoutePlanResponse"): return 10
        if ( name == "TaskTimingPair"): return 11
        if ( name == "BatchSummaryRequest"): return 12
        if ( name == "BatchSummaryResponse"): return 13
        if ( name == "TaskSummary"): return 14
        if ( name == "VehicleSummary"): return 15
        if ( name == "ImpactHeartbeat"): return 16
        if ( name == "ImpactComponentJoin"): return 17
        if ( name == "ImpactComponentLeave"): return 18
        if ( name == "SpeedAltPair"): return 19
        if ( name == "ImpactAutomationRequest"): return 20
        if ( name == "ImpactAutomationResponse"): return 21
        if ( name == "PointOfInterest"): return 22
        if ( name == "LineOfInterest"): return 23
        if ( name == "AreaOfInterest"): return 24
        if ( name == "ImpactPointSearchTask"): return 25
        if ( name == "PatternSearchTask"): return 26
        if ( name == "AngledAreaSearchTask"): return 27
        if ( name == "ImpactLineSearchTask"): return 28
        if ( name == "WatchTask"): return 29
        if ( name == "MultiVehicleWatchTask"): return 30
        if ( name == "CommRelayTask"): return 31
        if ( name == "CordonTask"): return 32
        if ( name == "BlockadeTask"): return 33
        if ( name == "EscortTask"): return 34
        if ( name == "ConfigurationRequest"): return 35
        if ( name == "WaterReport"): return 36
        if ( name == "WaterZone"): return 37

        return -1

    def getInstance(self, type):
        if(type ==  1): return PowerConfiguration.PowerConfiguration()
        if(type ==  2): return RadioConfiguration.RadioConfiguration()
        if(type ==  3): return RadioTowerConfiguration.RadioTowerConfiguration()
        if(type ==  4): return RadioState.RadioState()
        if(type ==  5): return RadioTowerState.RadioTowerState()
        if(type ==  6): return ImpactPayloadConfiguration.ImpactPayloadConfiguration()
        if(type ==  7): return DeployImpactPayload.DeployImpactPayload()
        if(type ==  8): return PowerPlantState.PowerPlantState()
        if(type ==  9): return BatchRoutePlanRequest.BatchRoutePlanRequest()
        if(type ==  10): return BatchRoutePlanResponse.BatchRoutePlanResponse()
        if(type ==  11): return TaskTimingPair.TaskTimingPair()
        if(type ==  12): return BatchSummaryRequest.BatchSummaryRequest()
        if(type ==  13): return BatchSummaryResponse.BatchSummaryResponse()
        if(type ==  14): return TaskSummary.TaskSummary()
        if(type ==  15): return VehicleSummary.VehicleSummary()
        if(type ==  16): return ImpactHeartbeat.ImpactHeartbeat()
        if(type ==  17): return ImpactComponentJoin.ImpactComponentJoin()
        if(type ==  18): return ImpactComponentLeave.ImpactComponentLeave()
        if(type ==  19): return SpeedAltPair.SpeedAltPair()
        if(type ==  20): return ImpactAutomationRequest.ImpactAutomationRequest()
        if(type ==  21): return ImpactAutomationResponse.ImpactAutomationResponse()
        if(type ==  22): return PointOfInterest.PointOfInterest()
        if(type ==  23): return LineOfInterest.LineOfInterest()
        if(type ==  24): return AreaOfInterest.AreaOfInterest()
        if(type ==  25): return ImpactPointSearchTask.ImpactPointSearchTask()
        if(type ==  26): return PatternSearchTask.PatternSearchTask()
        if(type ==  27): return AngledAreaSearchTask.AngledAreaSearchTask()
        if(type ==  28): return ImpactLineSearchTask.ImpactLineSearchTask()
        if(type ==  29): return WatchTask.WatchTask()
        if(type ==  30): return MultiVehicleWatchTask.MultiVehicleWatchTask()
        if(type ==  31): return CommRelayTask.CommRelayTask()
        if(type ==  32): return CordonTask.CordonTask()
        if(type ==  33): return BlockadeTask.BlockadeTask()
        if(type ==  34): return EscortTask.EscortTask()
        if(type ==  35): return ConfigurationRequest.ConfigurationRequest()
        if(type ==  36): return WaterReport.WaterReport()
        if(type ==  37): return WaterZone.WaterZone()

        return None
