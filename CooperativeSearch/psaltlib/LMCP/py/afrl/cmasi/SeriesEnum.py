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

import AbstractGeometry
import KeyValuePair
import Location3D
import PayloadAction
import PayloadConfiguration
import PayloadState
import VehicleAction
import Task
import SearchTask
import AbstractZone
import EntityConfiguration
import FlightProfile
import AirVehicleConfiguration
import EntityState
import AirVehicleState
import Wedge
import AreaSearchTask
import CameraAction
import CameraConfiguration
import GimballedPayloadState
import CameraState
import Circle
import GimbalAngleAction
import GimbalConfiguration
import GimbalScanAction
import GimbalStareAction
import GimbalState
import GoToWaypointAction
import KeepInZone
import KeepOutZone
import LineSearchTask
import NavigationAction
import LoiterAction
import LoiterTask
import Waypoint
import MissionCommand
import MustFlyTask
import OperatorSignal
import OperatingRegion
import AutomationRequest
import PointSearchTask
import Polygon
import Rectangle
import RemoveTasks
import ServiceStatus
import SessionStatus
import VehicleActionCommand
import VideoStreamAction
import VideoStreamConfiguration
import VideoStreamState
import AutomationResponse
import RemoveZones
import RemoveEntities
import FlightDirectorAction
import WeatherReport
import FollowPathCommand
import PathWaypoint
import StopMovementAction
import WaypointTransfer
import PayloadStowAction


SERIES_NAME = "CMASI"
#Series Name turned into a long for quick comparisons.
SERIES_NAME_ID = 4849604199710720000
SERIES_VERSION = 3


class SeriesEnum:

    def getName(self, type):
        if(type ==  1): return "AbstractGeometry"
        if(type ==  2): return "KeyValuePair"
        if(type ==  3): return "Location3D"
        if(type ==  4): return "PayloadAction"
        if(type ==  5): return "PayloadConfiguration"
        if(type ==  6): return "PayloadState"
        if(type ==  7): return "VehicleAction"
        if(type ==  8): return "Task"
        if(type ==  9): return "SearchTask"
        if(type ==  10): return "AbstractZone"
        if(type ==  11): return "EntityConfiguration"
        if(type ==  12): return "FlightProfile"
        if(type ==  13): return "AirVehicleConfiguration"
        if(type ==  14): return "EntityState"
        if(type ==  15): return "AirVehicleState"
        if(type ==  16): return "Wedge"
        if(type ==  17): return "AreaSearchTask"
        if(type ==  18): return "CameraAction"
        if(type ==  19): return "CameraConfiguration"
        if(type ==  20): return "GimballedPayloadState"
        if(type ==  21): return "CameraState"
        if(type ==  22): return "Circle"
        if(type ==  23): return "GimbalAngleAction"
        if(type ==  24): return "GimbalConfiguration"
        if(type ==  25): return "GimbalScanAction"
        if(type ==  26): return "GimbalStareAction"
        if(type ==  27): return "GimbalState"
        if(type ==  28): return "GoToWaypointAction"
        if(type ==  29): return "KeepInZone"
        if(type ==  30): return "KeepOutZone"
        if(type ==  31): return "LineSearchTask"
        if(type ==  32): return "NavigationAction"
        if(type ==  33): return "LoiterAction"
        if(type ==  34): return "LoiterTask"
        if(type ==  35): return "Waypoint"
        if(type ==  36): return "MissionCommand"
        if(type ==  37): return "MustFlyTask"
        if(type ==  38): return "OperatorSignal"
        if(type ==  39): return "OperatingRegion"
        if(type ==  40): return "AutomationRequest"
        if(type ==  41): return "PointSearchTask"
        if(type ==  42): return "Polygon"
        if(type ==  43): return "Rectangle"
        if(type ==  44): return "RemoveTasks"
        if(type ==  45): return "ServiceStatus"
        if(type ==  46): return "SessionStatus"
        if(type ==  47): return "VehicleActionCommand"
        if(type ==  48): return "VideoStreamAction"
        if(type ==  49): return "VideoStreamConfiguration"
        if(type ==  50): return "VideoStreamState"
        if(type ==  51): return "AutomationResponse"
        if(type ==  52): return "RemoveZones"
        if(type ==  53): return "RemoveEntities"
        if(type ==  54): return "FlightDirectorAction"
        if(type ==  55): return "WeatherReport"
        if(type ==  56): return "FollowPathCommand"
        if(type ==  57): return "PathWaypoint"
        if(type ==  58): return "StopMovementAction"
        if(type ==  59): return "WaypointTransfer"
        if(type ==  60): return "PayloadStowAction"


    def getType(self, name):
        if ( name == "AbstractGeometry"): return 1
        if ( name == "KeyValuePair"): return 2
        if ( name == "Location3D"): return 3
        if ( name == "PayloadAction"): return 4
        if ( name == "PayloadConfiguration"): return 5
        if ( name == "PayloadState"): return 6
        if ( name == "VehicleAction"): return 7
        if ( name == "Task"): return 8
        if ( name == "SearchTask"): return 9
        if ( name == "AbstractZone"): return 10
        if ( name == "EntityConfiguration"): return 11
        if ( name == "FlightProfile"): return 12
        if ( name == "AirVehicleConfiguration"): return 13
        if ( name == "EntityState"): return 14
        if ( name == "AirVehicleState"): return 15
        if ( name == "Wedge"): return 16
        if ( name == "AreaSearchTask"): return 17
        if ( name == "CameraAction"): return 18
        if ( name == "CameraConfiguration"): return 19
        if ( name == "GimballedPayloadState"): return 20
        if ( name == "CameraState"): return 21
        if ( name == "Circle"): return 22
        if ( name == "GimbalAngleAction"): return 23
        if ( name == "GimbalConfiguration"): return 24
        if ( name == "GimbalScanAction"): return 25
        if ( name == "GimbalStareAction"): return 26
        if ( name == "GimbalState"): return 27
        if ( name == "GoToWaypointAction"): return 28
        if ( name == "KeepInZone"): return 29
        if ( name == "KeepOutZone"): return 30
        if ( name == "LineSearchTask"): return 31
        if ( name == "NavigationAction"): return 32
        if ( name == "LoiterAction"): return 33
        if ( name == "LoiterTask"): return 34
        if ( name == "Waypoint"): return 35
        if ( name == "MissionCommand"): return 36
        if ( name == "MustFlyTask"): return 37
        if ( name == "OperatorSignal"): return 38
        if ( name == "OperatingRegion"): return 39
        if ( name == "AutomationRequest"): return 40
        if ( name == "PointSearchTask"): return 41
        if ( name == "Polygon"): return 42
        if ( name == "Rectangle"): return 43
        if ( name == "RemoveTasks"): return 44
        if ( name == "ServiceStatus"): return 45
        if ( name == "SessionStatus"): return 46
        if ( name == "VehicleActionCommand"): return 47
        if ( name == "VideoStreamAction"): return 48
        if ( name == "VideoStreamConfiguration"): return 49
        if ( name == "VideoStreamState"): return 50
        if ( name == "AutomationResponse"): return 51
        if ( name == "RemoveZones"): return 52
        if ( name == "RemoveEntities"): return 53
        if ( name == "FlightDirectorAction"): return 54
        if ( name == "WeatherReport"): return 55
        if ( name == "FollowPathCommand"): return 56
        if ( name == "PathWaypoint"): return 57
        if ( name == "StopMovementAction"): return 58
        if ( name == "WaypointTransfer"): return 59
        if ( name == "PayloadStowAction"): return 60

        return -1

    def getInstance(self, type):
        if(type ==  1): return AbstractGeometry.AbstractGeometry()
        if(type ==  2): return KeyValuePair.KeyValuePair()
        if(type ==  3): return Location3D.Location3D()
        if(type ==  4): return PayloadAction.PayloadAction()
        if(type ==  5): return PayloadConfiguration.PayloadConfiguration()
        if(type ==  6): return PayloadState.PayloadState()
        if(type ==  7): return VehicleAction.VehicleAction()
        if(type ==  8): return Task.Task()
        if(type ==  9): return SearchTask.SearchTask()
        if(type ==  10): return AbstractZone.AbstractZone()
        if(type ==  11): return EntityConfiguration.EntityConfiguration()
        if(type ==  12): return FlightProfile.FlightProfile()
        if(type ==  13): return AirVehicleConfiguration.AirVehicleConfiguration()
        if(type ==  14): return EntityState.EntityState()
        if(type ==  15): return AirVehicleState.AirVehicleState()
        if(type ==  16): return Wedge.Wedge()
        if(type ==  17): return AreaSearchTask.AreaSearchTask()
        if(type ==  18): return CameraAction.CameraAction()
        if(type ==  19): return CameraConfiguration.CameraConfiguration()
        if(type ==  20): return GimballedPayloadState.GimballedPayloadState()
        if(type ==  21): return CameraState.CameraState()
        if(type ==  22): return Circle.Circle()
        if(type ==  23): return GimbalAngleAction.GimbalAngleAction()
        if(type ==  24): return GimbalConfiguration.GimbalConfiguration()
        if(type ==  25): return GimbalScanAction.GimbalScanAction()
        if(type ==  26): return GimbalStareAction.GimbalStareAction()
        if(type ==  27): return GimbalState.GimbalState()
        if(type ==  28): return GoToWaypointAction.GoToWaypointAction()
        if(type ==  29): return KeepInZone.KeepInZone()
        if(type ==  30): return KeepOutZone.KeepOutZone()
        if(type ==  31): return LineSearchTask.LineSearchTask()
        if(type ==  32): return NavigationAction.NavigationAction()
        if(type ==  33): return LoiterAction.LoiterAction()
        if(type ==  34): return LoiterTask.LoiterTask()
        if(type ==  35): return Waypoint.Waypoint()
        if(type ==  36): return MissionCommand.MissionCommand()
        if(type ==  37): return MustFlyTask.MustFlyTask()
        if(type ==  38): return OperatorSignal.OperatorSignal()
        if(type ==  39): return OperatingRegion.OperatingRegion()
        if(type ==  40): return AutomationRequest.AutomationRequest()
        if(type ==  41): return PointSearchTask.PointSearchTask()
        if(type ==  42): return Polygon.Polygon()
        if(type ==  43): return Rectangle.Rectangle()
        if(type ==  44): return RemoveTasks.RemoveTasks()
        if(type ==  45): return ServiceStatus.ServiceStatus()
        if(type ==  46): return SessionStatus.SessionStatus()
        if(type ==  47): return VehicleActionCommand.VehicleActionCommand()
        if(type ==  48): return VideoStreamAction.VideoStreamAction()
        if(type ==  49): return VideoStreamConfiguration.VideoStreamConfiguration()
        if(type ==  50): return VideoStreamState.VideoStreamState()
        if(type ==  51): return AutomationResponse.AutomationResponse()
        if(type ==  52): return RemoveZones.RemoveZones()
        if(type ==  53): return RemoveEntities.RemoveEntities()
        if(type ==  54): return FlightDirectorAction.FlightDirectorAction()
        if(type ==  55): return WeatherReport.WeatherReport()
        if(type ==  56): return FollowPathCommand.FollowPathCommand()
        if(type ==  57): return PathWaypoint.PathWaypoint()
        if(type ==  58): return StopMovementAction.StopMovementAction()
        if(type ==  59): return WaypointTransfer.WaypointTransfer()
        if(type ==  60): return PayloadStowAction.PayloadStowAction()

        return None
