import socket
from lmcp import LMCPFactory

## ===============================================================================
## Authors: AFRL/RQQA
## Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
## 
## Copyright (c) 2017 Government of the United State of America, as represented by
## the Secretary of the Air Force.  No copyright is claimed in the United States under
## Title 17, U.S. Code.  All Other Rights Reserved.
## ===============================================================================

## This file was auto-created by LmcpGen. Modifications will be overwritten.

from afrl.cmasi import AbstractGeometry
from afrl.cmasi import KeyValuePair
from afrl.cmasi import Location3D
from afrl.cmasi import PayloadAction
from afrl.cmasi import PayloadConfiguration
from afrl.cmasi import PayloadState
from afrl.cmasi import VehicleAction
from afrl.cmasi import Task
from afrl.cmasi import SearchTask
from afrl.cmasi import AbstractZone
from afrl.cmasi import EntityConfiguration
from afrl.cmasi import FlightProfile
from afrl.cmasi import AirVehicleConfiguration
from afrl.cmasi import EntityState
from afrl.cmasi import AirVehicleState
from afrl.cmasi import Wedge
from afrl.cmasi import AreaSearchTask
from afrl.cmasi import CameraAction
from afrl.cmasi import CameraConfiguration
from afrl.cmasi import GimballedPayloadState
from afrl.cmasi import CameraState
from afrl.cmasi import Circle
from afrl.cmasi import GimbalAngleAction
from afrl.cmasi import GimbalConfiguration
from afrl.cmasi import GimbalScanAction
from afrl.cmasi import GimbalStareAction
from afrl.cmasi import GimbalState
from afrl.cmasi import GoToWaypointAction
from afrl.cmasi import KeepInZone
from afrl.cmasi import KeepOutZone
from afrl.cmasi import LineSearchTask
from afrl.cmasi import NavigationAction
from afrl.cmasi import LoiterAction
from afrl.cmasi import LoiterTask
from afrl.cmasi import Waypoint
from afrl.cmasi import MissionCommand
from afrl.cmasi import MustFlyTask
from afrl.cmasi import OperatorSignal
from afrl.cmasi import OperatingRegion
from afrl.cmasi import AutomationRequest
from afrl.cmasi import PointSearchTask
from afrl.cmasi import Polygon
from afrl.cmasi import Rectangle
from afrl.cmasi import RemoveTasks
from afrl.cmasi import ServiceStatus
from afrl.cmasi import SessionStatus
from afrl.cmasi import VehicleActionCommand
from afrl.cmasi import VideoStreamAction
from afrl.cmasi import VideoStreamConfiguration
from afrl.cmasi import VideoStreamState
from afrl.cmasi import AutomationResponse
from afrl.cmasi import RemoveZones
from afrl.cmasi import RemoveEntities
from afrl.cmasi import FlightDirectorAction
from afrl.cmasi import WeatherReport
from afrl.cmasi import FollowPathCommand
from afrl.cmasi import PathWaypoint
from afrl.cmasi import StopMovementAction
from afrl.cmasi import WaypointTransfer
from afrl.cmasi import PayloadStowAction
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
from afrl.cmasi.perceive import EntityPerception
from afrl.cmasi.perceive import TrackEntityAction
from afrl.cmasi.perceive import TrackEntityTask
from uxas.messages.route import GraphNode
from uxas.messages.route import GraphEdge
from uxas.messages.route import GraphRegion
from uxas.messages.route import RouteConstraints
from uxas.messages.route import RouteRequest
from uxas.messages.route import RoutePlanRequest
from uxas.messages.route import RoutePlan
from uxas.messages.route import RoutePlanResponse
from uxas.messages.route import RouteResponse
from uxas.messages.route import EgressRouteRequest
from uxas.messages.route import EgressRouteResponse
from uxas.messages.route import RoadPointsConstraints
from uxas.messages.route import RoadPointsRequest
from uxas.messages.route import RoadPointsResponse
from uxas.messages.uxnative import VideoRecord
from uxas.messages.uxnative import StartupComplete
from uxas.messages.uxnative import CreateNewService
from uxas.messages.uxnative import KillService
from uxas.messages.uxnative import IncrementWaypoint
from uxas.messages.uxnative import EntityLocation
from uxas.messages.uxnative import BandwidthTest
from uxas.messages.uxnative import BandwidthReceiveReport
from uxas.messages.uxnative import SubTaskExecution
from uxas.messages.uxnative import SubTaskAssignment
from uxas.messages.uxnative import AutopilotKeepAlive
from uxas.messages.uxnative import OnboardProcessorIsAlive
from uxas.messages.uxnative import EntityJoin
from uxas.messages.uxnative import EntityExit
from uxas.messages.task import AssignmentCoordinatorTask
from uxas.messages.task import RendezvousTask
from uxas.messages.task import PlanningState
from uxas.messages.task import AssignmentCoordination
from uxas.messages.task import CoordinatedAutomationRequest
from uxas.messages.task import TaskAutomationRequest
from uxas.messages.task import TaskAutomationResponse
from uxas.messages.task import UniqueAutomationRequest
from uxas.messages.task import UniqueAutomationResponse
from uxas.messages.task import SensorFootprintRequests
from uxas.messages.task import FootprintRequest
from uxas.messages.task import SensorFootprint
from uxas.messages.task import SensorFootprintResponse
from uxas.messages.task import TaskImplementationRequest
from uxas.messages.task import TaskImplementationResponse
from uxas.messages.task import AssignmentCostMatrix
from uxas.messages.task import TaskOptionCost
from uxas.messages.task import TaskAssignment
from uxas.messages.task import TaskAssignmentSummary
from uxas.messages.task import TaskOption
from uxas.messages.task import TaskPlanOptions
from uxas.messages.task import TaskPause
from uxas.messages.task import TaskResume
from uxas.messages.task import TaskProgress
from uxas.messages.task import TaskProgressRequest
from uxas.messages.task import TaskInitialized
from uxas.messages.task import TaskActive
from uxas.messages.task import TaskComplete
from uxas.messages.task import CancelTask
from afrl.vehicles import GroundVehicleConfiguration
from afrl.vehicles import GroundVehicleState
from afrl.vehicles import SurfaceVehicleConfiguration
from afrl.vehicles import SurfaceVehicleState
from afrl.vehicles import StationarySensorConfiguration
from afrl.vehicles import StationarySensorState


s = socket.socket()
host = socket.gethostname()
port = 11041
s.connect((host, port))
buf = []

#Pack AbstractGeometry
obj = AbstractGeometry.AbstractGeometry()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack KeyValuePair
obj = KeyValuePair.KeyValuePair()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack Location3D
obj = Location3D.Location3D()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack PayloadAction
obj = PayloadAction.PayloadAction()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack PayloadConfiguration
obj = PayloadConfiguration.PayloadConfiguration()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack PayloadState
obj = PayloadState.PayloadState()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack VehicleAction
obj = VehicleAction.VehicleAction()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack Task
obj = Task.Task()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack SearchTask
obj = SearchTask.SearchTask()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack AbstractZone
obj = AbstractZone.AbstractZone()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack EntityConfiguration
obj = EntityConfiguration.EntityConfiguration()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack FlightProfile
obj = FlightProfile.FlightProfile()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack AirVehicleConfiguration
obj = AirVehicleConfiguration.AirVehicleConfiguration()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack EntityState
obj = EntityState.EntityState()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack AirVehicleState
obj = AirVehicleState.AirVehicleState()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack Wedge
obj = Wedge.Wedge()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack AreaSearchTask
obj = AreaSearchTask.AreaSearchTask()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack CameraAction
obj = CameraAction.CameraAction()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack CameraConfiguration
obj = CameraConfiguration.CameraConfiguration()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack GimballedPayloadState
obj = GimballedPayloadState.GimballedPayloadState()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack CameraState
obj = CameraState.CameraState()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack Circle
obj = Circle.Circle()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack GimbalAngleAction
obj = GimbalAngleAction.GimbalAngleAction()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack GimbalConfiguration
obj = GimbalConfiguration.GimbalConfiguration()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack GimbalScanAction
obj = GimbalScanAction.GimbalScanAction()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack GimbalStareAction
obj = GimbalStareAction.GimbalStareAction()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack GimbalState
obj = GimbalState.GimbalState()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack GoToWaypointAction
obj = GoToWaypointAction.GoToWaypointAction()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack KeepInZone
obj = KeepInZone.KeepInZone()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack KeepOutZone
obj = KeepOutZone.KeepOutZone()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack LineSearchTask
obj = LineSearchTask.LineSearchTask()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack NavigationAction
obj = NavigationAction.NavigationAction()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack LoiterAction
obj = LoiterAction.LoiterAction()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack LoiterTask
obj = LoiterTask.LoiterTask()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack Waypoint
obj = Waypoint.Waypoint()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack MissionCommand
obj = MissionCommand.MissionCommand()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack MustFlyTask
obj = MustFlyTask.MustFlyTask()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack OperatorSignal
obj = OperatorSignal.OperatorSignal()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack OperatingRegion
obj = OperatingRegion.OperatingRegion()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack AutomationRequest
obj = AutomationRequest.AutomationRequest()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack PointSearchTask
obj = PointSearchTask.PointSearchTask()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack Polygon
obj = Polygon.Polygon()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack Rectangle
obj = Rectangle.Rectangle()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack RemoveTasks
obj = RemoveTasks.RemoveTasks()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack ServiceStatus
obj = ServiceStatus.ServiceStatus()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack SessionStatus
obj = SessionStatus.SessionStatus()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack VehicleActionCommand
obj = VehicleActionCommand.VehicleActionCommand()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack VideoStreamAction
obj = VideoStreamAction.VideoStreamAction()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack VideoStreamConfiguration
obj = VideoStreamConfiguration.VideoStreamConfiguration()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack VideoStreamState
obj = VideoStreamState.VideoStreamState()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack AutomationResponse
obj = AutomationResponse.AutomationResponse()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack RemoveZones
obj = RemoveZones.RemoveZones()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack RemoveEntities
obj = RemoveEntities.RemoveEntities()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack FlightDirectorAction
obj = FlightDirectorAction.FlightDirectorAction()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack WeatherReport
obj = WeatherReport.WeatherReport()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack FollowPathCommand
obj = FollowPathCommand.FollowPathCommand()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack PathWaypoint
obj = PathWaypoint.PathWaypoint()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack StopMovementAction
obj = StopMovementAction.StopMovementAction()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack WaypointTransfer
obj = WaypointTransfer.WaypointTransfer()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack PayloadStowAction
obj = PayloadStowAction.PayloadStowAction()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack PowerConfiguration
obj = PowerConfiguration.PowerConfiguration()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack RadioConfiguration
obj = RadioConfiguration.RadioConfiguration()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack RadioTowerConfiguration
obj = RadioTowerConfiguration.RadioTowerConfiguration()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack RadioState
obj = RadioState.RadioState()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack RadioTowerState
obj = RadioTowerState.RadioTowerState()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack ImpactPayloadConfiguration
obj = ImpactPayloadConfiguration.ImpactPayloadConfiguration()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack DeployImpactPayload
obj = DeployImpactPayload.DeployImpactPayload()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack PowerPlantState
obj = PowerPlantState.PowerPlantState()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack BatchRoutePlanRequest
obj = BatchRoutePlanRequest.BatchRoutePlanRequest()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack BatchRoutePlanResponse
obj = BatchRoutePlanResponse.BatchRoutePlanResponse()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack TaskTimingPair
obj = TaskTimingPair.TaskTimingPair()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack BatchSummaryRequest
obj = BatchSummaryRequest.BatchSummaryRequest()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack BatchSummaryResponse
obj = BatchSummaryResponse.BatchSummaryResponse()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack TaskSummary
obj = TaskSummary.TaskSummary()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack VehicleSummary
obj = VehicleSummary.VehicleSummary()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack ImpactHeartbeat
obj = ImpactHeartbeat.ImpactHeartbeat()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack ImpactComponentJoin
obj = ImpactComponentJoin.ImpactComponentJoin()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack ImpactComponentLeave
obj = ImpactComponentLeave.ImpactComponentLeave()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack SpeedAltPair
obj = SpeedAltPair.SpeedAltPair()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack ImpactAutomationRequest
obj = ImpactAutomationRequest.ImpactAutomationRequest()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack ImpactAutomationResponse
obj = ImpactAutomationResponse.ImpactAutomationResponse()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack PointOfInterest
obj = PointOfInterest.PointOfInterest()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack LineOfInterest
obj = LineOfInterest.LineOfInterest()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack AreaOfInterest
obj = AreaOfInterest.AreaOfInterest()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack ImpactPointSearchTask
obj = ImpactPointSearchTask.ImpactPointSearchTask()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack PatternSearchTask
obj = PatternSearchTask.PatternSearchTask()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack AngledAreaSearchTask
obj = AngledAreaSearchTask.AngledAreaSearchTask()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack ImpactLineSearchTask
obj = ImpactLineSearchTask.ImpactLineSearchTask()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack WatchTask
obj = WatchTask.WatchTask()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack MultiVehicleWatchTask
obj = MultiVehicleWatchTask.MultiVehicleWatchTask()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack CommRelayTask
obj = CommRelayTask.CommRelayTask()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack CordonTask
obj = CordonTask.CordonTask()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack BlockadeTask
obj = BlockadeTask.BlockadeTask()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack EscortTask
obj = EscortTask.EscortTask()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack ConfigurationRequest
obj = ConfigurationRequest.ConfigurationRequest()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack WaterReport
obj = WaterReport.WaterReport()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack WaterZone
obj = WaterZone.WaterZone()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack EntityPerception
obj = EntityPerception.EntityPerception()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack TrackEntityAction
obj = TrackEntityAction.TrackEntityAction()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack TrackEntityTask
obj = TrackEntityTask.TrackEntityTask()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack GraphNode
obj = GraphNode.GraphNode()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack GraphEdge
obj = GraphEdge.GraphEdge()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack GraphRegion
obj = GraphRegion.GraphRegion()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack RouteConstraints
obj = RouteConstraints.RouteConstraints()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack RouteRequest
obj = RouteRequest.RouteRequest()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack RoutePlanRequest
obj = RoutePlanRequest.RoutePlanRequest()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack RoutePlan
obj = RoutePlan.RoutePlan()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack RoutePlanResponse
obj = RoutePlanResponse.RoutePlanResponse()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack RouteResponse
obj = RouteResponse.RouteResponse()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack EgressRouteRequest
obj = EgressRouteRequest.EgressRouteRequest()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack EgressRouteResponse
obj = EgressRouteResponse.EgressRouteResponse()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack RoadPointsConstraints
obj = RoadPointsConstraints.RoadPointsConstraints()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack RoadPointsRequest
obj = RoadPointsRequest.RoadPointsRequest()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack RoadPointsResponse
obj = RoadPointsResponse.RoadPointsResponse()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack VideoRecord
obj = VideoRecord.VideoRecord()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack StartupComplete
obj = StartupComplete.StartupComplete()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack CreateNewService
obj = CreateNewService.CreateNewService()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack KillService
obj = KillService.KillService()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack IncrementWaypoint
obj = IncrementWaypoint.IncrementWaypoint()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack EntityLocation
obj = EntityLocation.EntityLocation()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack BandwidthTest
obj = BandwidthTest.BandwidthTest()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack BandwidthReceiveReport
obj = BandwidthReceiveReport.BandwidthReceiveReport()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack SubTaskExecution
obj = SubTaskExecution.SubTaskExecution()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack SubTaskAssignment
obj = SubTaskAssignment.SubTaskAssignment()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack AutopilotKeepAlive
obj = AutopilotKeepAlive.AutopilotKeepAlive()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack OnboardProcessorIsAlive
obj = OnboardProcessorIsAlive.OnboardProcessorIsAlive()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack EntityJoin
obj = EntityJoin.EntityJoin()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack EntityExit
obj = EntityExit.EntityExit()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack AssignmentCoordinatorTask
obj = AssignmentCoordinatorTask.AssignmentCoordinatorTask()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack RendezvousTask
obj = RendezvousTask.RendezvousTask()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack PlanningState
obj = PlanningState.PlanningState()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack AssignmentCoordination
obj = AssignmentCoordination.AssignmentCoordination()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack CoordinatedAutomationRequest
obj = CoordinatedAutomationRequest.CoordinatedAutomationRequest()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack TaskAutomationRequest
obj = TaskAutomationRequest.TaskAutomationRequest()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack TaskAutomationResponse
obj = TaskAutomationResponse.TaskAutomationResponse()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack UniqueAutomationRequest
obj = UniqueAutomationRequest.UniqueAutomationRequest()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack UniqueAutomationResponse
obj = UniqueAutomationResponse.UniqueAutomationResponse()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack SensorFootprintRequests
obj = SensorFootprintRequests.SensorFootprintRequests()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack FootprintRequest
obj = FootprintRequest.FootprintRequest()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack SensorFootprint
obj = SensorFootprint.SensorFootprint()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack SensorFootprintResponse
obj = SensorFootprintResponse.SensorFootprintResponse()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack TaskImplementationRequest
obj = TaskImplementationRequest.TaskImplementationRequest()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack TaskImplementationResponse
obj = TaskImplementationResponse.TaskImplementationResponse()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack AssignmentCostMatrix
obj = AssignmentCostMatrix.AssignmentCostMatrix()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack TaskOptionCost
obj = TaskOptionCost.TaskOptionCost()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack TaskAssignment
obj = TaskAssignment.TaskAssignment()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack TaskAssignmentSummary
obj = TaskAssignmentSummary.TaskAssignmentSummary()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack TaskOption
obj = TaskOption.TaskOption()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack TaskPlanOptions
obj = TaskPlanOptions.TaskPlanOptions()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack TaskPause
obj = TaskPause.TaskPause()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack TaskResume
obj = TaskResume.TaskResume()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack TaskProgress
obj = TaskProgress.TaskProgress()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack TaskProgressRequest
obj = TaskProgressRequest.TaskProgressRequest()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack TaskInitialized
obj = TaskInitialized.TaskInitialized()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack TaskActive
obj = TaskActive.TaskActive()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack TaskComplete
obj = TaskComplete.TaskComplete()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack CancelTask
obj = CancelTask.CancelTask()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack GroundVehicleConfiguration
obj = GroundVehicleConfiguration.GroundVehicleConfiguration()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack GroundVehicleState
obj = GroundVehicleState.GroundVehicleState()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack SurfaceVehicleConfiguration
obj = SurfaceVehicleConfiguration.SurfaceVehicleConfiguration()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack SurfaceVehicleState
obj = SurfaceVehicleState.SurfaceVehicleState()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack StationarySensorConfiguration
obj = StationarySensorConfiguration.StationarySensorConfiguration()
buf.append(LMCPFactory.packMessage(obj, True))
#Pack StationarySensorState
obj = StationarySensorState.StationarySensorState()
buf.append(LMCPFactory.packMessage(obj, True))


s.send("".join(buf))


