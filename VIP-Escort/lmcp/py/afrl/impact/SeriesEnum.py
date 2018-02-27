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

from afrl.impact import PowerConfiguration
from afrl.impact import RadioConfiguration
from afrl.impact import RadioTowerConfiguration
from afrl.impact import RadioState
from afrl.impact import RadioTowerState
from afrl.impact import ImpactPayloadConfiguration
from afrl.impact import DeployImpactPayload
from afrl.impact import PowerPlantState
from afrl.impact import BatchRoutePlanRequest
from afrl.impact import BatchRoutePlanResponse
from afrl.impact import TaskTimingPair
from afrl.impact import BatchSummaryRequest
from afrl.impact import BatchSummaryResponse
from afrl.impact import TaskSummary
from afrl.impact import VehicleSummary
from afrl.impact import ImpactHeartbeat
from afrl.impact import ImpactComponentJoin
from afrl.impact import ImpactComponentLeave
from afrl.impact import SpeedAltPair
from afrl.impact import ImpactAutomationRequest
from afrl.impact import ImpactAutomationResponse
from afrl.impact import PointOfInterest
from afrl.impact import LineOfInterest
from afrl.impact import AreaOfInterest
from afrl.impact import ImpactPointSearchTask
from afrl.impact import PatternSearchTask
from afrl.impact import AngledAreaSearchTask
from afrl.impact import ImpactLineSearchTask
from afrl.impact import WatchTask
from afrl.impact import MultiVehicleWatchTask
from afrl.impact import CommRelayTask
from afrl.impact import CordonTask
from afrl.impact import BlockadeTask
from afrl.impact import EscortTask
from afrl.impact import ConfigurationRequest
from afrl.impact import WaterReport
from afrl.impact import WaterZone


SERIES_NAME = "IMPACT"
#Series Name turned into a long for quick comparisons.
SERIES_NAME_ID = 5281966179208134656
SERIES_VERSION = 13


class SeriesEnum:

    def getName(self, type_):
        if(type_ ==  1): return "PowerConfiguration"
        if(type_ ==  2): return "RadioConfiguration"
        if(type_ ==  3): return "RadioTowerConfiguration"
        if(type_ ==  4): return "RadioState"
        if(type_ ==  5): return "RadioTowerState"
        if(type_ ==  6): return "ImpactPayloadConfiguration"
        if(type_ ==  7): return "DeployImpactPayload"
        if(type_ ==  8): return "PowerPlantState"
        if(type_ ==  9): return "BatchRoutePlanRequest"
        if(type_ ==  10): return "BatchRoutePlanResponse"
        if(type_ ==  11): return "TaskTimingPair"
        if(type_ ==  12): return "BatchSummaryRequest"
        if(type_ ==  13): return "BatchSummaryResponse"
        if(type_ ==  14): return "TaskSummary"
        if(type_ ==  15): return "VehicleSummary"
        if(type_ ==  16): return "ImpactHeartbeat"
        if(type_ ==  17): return "ImpactComponentJoin"
        if(type_ ==  18): return "ImpactComponentLeave"
        if(type_ ==  19): return "SpeedAltPair"
        if(type_ ==  20): return "ImpactAutomationRequest"
        if(type_ ==  21): return "ImpactAutomationResponse"
        if(type_ ==  22): return "PointOfInterest"
        if(type_ ==  23): return "LineOfInterest"
        if(type_ ==  24): return "AreaOfInterest"
        if(type_ ==  25): return "ImpactPointSearchTask"
        if(type_ ==  26): return "PatternSearchTask"
        if(type_ ==  27): return "AngledAreaSearchTask"
        if(type_ ==  28): return "ImpactLineSearchTask"
        if(type_ ==  29): return "WatchTask"
        if(type_ ==  30): return "MultiVehicleWatchTask"
        if(type_ ==  31): return "CommRelayTask"
        if(type_ ==  32): return "CordonTask"
        if(type_ ==  33): return "BlockadeTask"
        if(type_ ==  34): return "EscortTask"
        if(type_ ==  35): return "ConfigurationRequest"
        if(type_ ==  36): return "WaterReport"
        if(type_ ==  37): return "WaterZone"


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

    def getInstance(self, type_):
        if(type_ ==  1): return PowerConfiguration.PowerConfiguration()
        if(type_ ==  2): return RadioConfiguration.RadioConfiguration()
        if(type_ ==  3): return RadioTowerConfiguration.RadioTowerConfiguration()
        if(type_ ==  4): return RadioState.RadioState()
        if(type_ ==  5): return RadioTowerState.RadioTowerState()
        if(type_ ==  6): return ImpactPayloadConfiguration.ImpactPayloadConfiguration()
        if(type_ ==  7): return DeployImpactPayload.DeployImpactPayload()
        if(type_ ==  8): return PowerPlantState.PowerPlantState()
        if(type_ ==  9): return BatchRoutePlanRequest.BatchRoutePlanRequest()
        if(type_ ==  10): return BatchRoutePlanResponse.BatchRoutePlanResponse()
        if(type_ ==  11): return TaskTimingPair.TaskTimingPair()
        if(type_ ==  12): return BatchSummaryRequest.BatchSummaryRequest()
        if(type_ ==  13): return BatchSummaryResponse.BatchSummaryResponse()
        if(type_ ==  14): return TaskSummary.TaskSummary()
        if(type_ ==  15): return VehicleSummary.VehicleSummary()
        if(type_ ==  16): return ImpactHeartbeat.ImpactHeartbeat()
        if(type_ ==  17): return ImpactComponentJoin.ImpactComponentJoin()
        if(type_ ==  18): return ImpactComponentLeave.ImpactComponentLeave()
        if(type_ ==  19): return SpeedAltPair.SpeedAltPair()
        if(type_ ==  20): return ImpactAutomationRequest.ImpactAutomationRequest()
        if(type_ ==  21): return ImpactAutomationResponse.ImpactAutomationResponse()
        if(type_ ==  22): return PointOfInterest.PointOfInterest()
        if(type_ ==  23): return LineOfInterest.LineOfInterest()
        if(type_ ==  24): return AreaOfInterest.AreaOfInterest()
        if(type_ ==  25): return ImpactPointSearchTask.ImpactPointSearchTask()
        if(type_ ==  26): return PatternSearchTask.PatternSearchTask()
        if(type_ ==  27): return AngledAreaSearchTask.AngledAreaSearchTask()
        if(type_ ==  28): return ImpactLineSearchTask.ImpactLineSearchTask()
        if(type_ ==  29): return WatchTask.WatchTask()
        if(type_ ==  30): return MultiVehicleWatchTask.MultiVehicleWatchTask()
        if(type_ ==  31): return CommRelayTask.CommRelayTask()
        if(type_ ==  32): return CordonTask.CordonTask()
        if(type_ ==  33): return BlockadeTask.BlockadeTask()
        if(type_ ==  34): return EscortTask.EscortTask()
        if(type_ ==  35): return ConfigurationRequest.ConfigurationRequest()
        if(type_ ==  36): return WaterReport.WaterReport()
        if(type_ ==  37): return WaterZone.WaterZone()

        return None
