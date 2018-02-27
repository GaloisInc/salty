import sys
sys.path.insert(0, '../lmcp/LMCP/py')
from lmcp import LMCPFactory

# User defined imports
import LocalCoords, math


class AvState:

    shared_state = dict()

    def __init__(self, AirVehicleConfiguration, AirVehicleState):
        self.ID = AirVehicleConfiguration.get_ID()
        self.EnergyAvailable = AirVehicleState.get_EnergyAvailable
        self.Latitude = AirVehicleState.get_Location().get_Latitude()
        self.Longitude = AirVehicleState.get_Location().get_Longitude()
        self.Altitude = AirVehicleState.get_Location().get_Altitude()
        self.AirVehicleState = AirVehicleState
        self.AirVehicleConfiguration = AirVehicleConfiguration
        self.internal_state = dict()

    def update_state(self, AirVehicleState):
        self.EnergyAvailable = AirVehicleState.get_EnergyAvailable()
        self.Latitude = AirVehicleState.get_Location().get_Latitude()
        self.Longitude = AirVehicleState.get_Location().get_Longitude()
        self.Altitude = AirVehicleState.get_Location().get_Altitude()
        self.AirVehicleState = AirVehicleState

    def distance(self, AvState_2):
        north_east_m_xy = LocalCoords.LatLong_degToNorthEast_m(self.Latitude, self.Longitude)
        north_east_m_xy2 = LocalCoords.LatLong_degToNorthEast_m(AvState_2.Latitude, AvState_2.Longitude)
        distance = math.sqrt((north_east_m_xy[0] - north_east_m_xy2[0])**2 +
                             (north_east_m_xy[1] - north_east_m_xy2[1])**2)
        return distance
