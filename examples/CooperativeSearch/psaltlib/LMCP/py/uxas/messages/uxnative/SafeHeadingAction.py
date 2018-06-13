#! /usr/bin/python

import sys, struct
import xml.dom.minidom
from lmcp import LMCPObject

## ===============================================================================
## Authors: AFRL/RQQA
## Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
## 
## Copyright (c) 2017 Government of the United State of America, as represented by
## the Secretary of the Air Force.  No copyright is claimed in the United States under
## Title 17, U.S. Code.  All Other Rights Reserved.
## ===============================================================================

## This file was auto-created by LmcpGen. Modifications will be overwritten.

from afrl.cmasi import VehicleAction


class SafeHeadingAction(VehicleAction.VehicleAction):

    def __init__(self):
        VehicleAction.VehicleAction.__init__(self)
        self.LMCP_TYPE = 6
        self.SERIES_NAME = "UXNATIVE"
        self.FULL_LMCP_TYPE_NAME = "uxas.messages.uxnative.SafeHeadingAction"
        #Series Name turned into a long for quick comparisons.
        self.SERIES_NAME_ID = 6149751333668345413
        self.SERIES_VERSION = 4

        #Define message fields
        self.VehicleID = 0   #int64
        self.OperatingRegion = 0   #int64
        self.LeadAheadDistance = 1000.0   #real32
        self.LoiterRadius = 0.0   #real32
        self.DesiredHeading = 0   #real32
        self.DesiredHeadingRate = 0   #real32
        self.UseHeadingRate = False   #bool


    def pack(self):
        """
        Packs the object data and returns a string that contains all of the serialized
        members.
        """
        buffer = bytearray()
        buffer.extend(VehicleAction.VehicleAction.pack(self))
        buffer.extend(struct.pack(">q", self.VehicleID))
        buffer.extend(struct.pack(">q", self.OperatingRegion))
        buffer.extend(struct.pack(">f", self.LeadAheadDistance))
        buffer.extend(struct.pack(">f", self.LoiterRadius))
        buffer.extend(struct.pack(">f", self.DesiredHeading))
        buffer.extend(struct.pack(">f", self.DesiredHeadingRate))
        boolChar = 1 if self.UseHeadingRate == True else 0
        buffer.extend(struct.pack(">B",boolChar))

        return buffer

    def unpack(self, buffer, _pos):
        """
        Unpacks data from a bytearray and sets class members
        """
        _pos = VehicleAction.VehicleAction.unpack(self, buffer, _pos)
        self.VehicleID = struct.unpack_from(">q", buffer, _pos)[0]
        _pos += 8
        self.OperatingRegion = struct.unpack_from(">q", buffer, _pos)[0]
        _pos += 8
        self.LeadAheadDistance = struct.unpack_from(">f", buffer, _pos)[0]
        _pos += 4
        self.LoiterRadius = struct.unpack_from(">f", buffer, _pos)[0]
        _pos += 4
        self.DesiredHeading = struct.unpack_from(">f", buffer, _pos)[0]
        _pos += 4
        self.DesiredHeadingRate = struct.unpack_from(">f", buffer, _pos)[0]
        _pos += 4
        boolChar = struct.unpack_from(">B", buffer, _pos)[0]
        self.UseHeadingRate = True if boolChar == 1 else False
        _pos += 1
        return _pos


    def unpackFromXMLNode(self, el, seriesFactory):
        VehicleAction.VehicleAction.unpackFromXMLNode(self, el, seriesFactory)
        for e in el.childNodes:
            if e.nodeType == xml.dom.Node.ELEMENT_NODE:
                if e.localName == "VehicleID" and len(e.childNodes) > 0 :
                    self.VehicleID = int(e.childNodes[0].nodeValue)
                elif e.localName == "OperatingRegion" and len(e.childNodes) > 0 :
                    self.OperatingRegion = int(e.childNodes[0].nodeValue)
                elif e.localName == "LeadAheadDistance" and len(e.childNodes) > 0 :
                    self.LeadAheadDistance = float(e.childNodes[0].nodeValue)
                elif e.localName == "LoiterRadius" and len(e.childNodes) > 0 :
                    self.LoiterRadius = float(e.childNodes[0].nodeValue)
                elif e.localName == "DesiredHeading" and len(e.childNodes) > 0 :
                    self.DesiredHeading = float(e.childNodes[0].nodeValue)
                elif e.localName == "DesiredHeadingRate" and len(e.childNodes) > 0 :
                    self.DesiredHeadingRate = float(e.childNodes[0].nodeValue)
                elif e.localName == "UseHeadingRate" and len(e.childNodes) > 0 :
                    self.UseHeadingRate = e.childNodes[0].nodeValue.lower() == 'true'

        return

    def unpackFromDict(self, d, seriesFactory):
        VehicleAction.VehicleAction.unpackFromDict(self, d, seriesFactory)
        for key in d:
            if key == "VehicleID":
                self.VehicleID = d[key]
            elif key == "OperatingRegion":
                self.OperatingRegion = d[key]
            elif key == "LeadAheadDistance":
                self.LeadAheadDistance = d[key]
            elif key == "LoiterRadius":
                self.LoiterRadius = d[key]
            elif key == "DesiredHeading":
                self.DesiredHeading = d[key]
            elif key == "DesiredHeadingRate":
                self.DesiredHeadingRate = d[key]
            elif key == "UseHeadingRate":
                self.UseHeadingRate = d[key]

        return

    def get_VehicleID(self):
        return self.VehicleID

    def set_VehicleID(self, value):
        self.VehicleID = int( value )

    def get_OperatingRegion(self):
        return self.OperatingRegion

    def set_OperatingRegion(self, value):
        self.OperatingRegion = int( value )

    def get_LeadAheadDistance(self):
        return self.LeadAheadDistance

    def set_LeadAheadDistance(self, value):
        self.LeadAheadDistance = float( value )

    def get_LoiterRadius(self):
        return self.LoiterRadius

    def set_LoiterRadius(self, value):
        self.LoiterRadius = float( value )

    def get_DesiredHeading(self):
        return self.DesiredHeading

    def set_DesiredHeading(self, value):
        self.DesiredHeading = float( value )

    def get_DesiredHeadingRate(self):
        return self.DesiredHeadingRate

    def set_DesiredHeadingRate(self, value):
        self.DesiredHeadingRate = float( value )

    def get_UseHeadingRate(self):
        return self.UseHeadingRate

    def set_UseHeadingRate(self, value):
        self.UseHeadingRate = bool( value )



    def toString(self):
        """
        Returns a string representation of all variables
        """
        buf = VehicleAction.VehicleAction.toString(self)
        buf += "From SafeHeadingAction:\n"
        buf +=    "VehicleID = " + str( self.VehicleID ) + "\n" 
        buf +=    "OperatingRegion = " + str( self.OperatingRegion ) + "\n" 
        buf +=    "LeadAheadDistance = " + str( self.LeadAheadDistance ) + "\n" 
        buf +=    "LoiterRadius = " + str( self.LoiterRadius ) + "\n" 
        buf +=    "DesiredHeading = " + str( self.DesiredHeading ) + "\n" 
        buf +=    "DesiredHeadingRate = " + str( self.DesiredHeadingRate ) + "\n" 
        buf +=    "UseHeadingRate = " + str( self.UseHeadingRate ) + "\n" 

        return buf;

    def toDict(self):
        m = {}
        self.toDictMembers(m)
        d = {}
        if ("UXNATIVE" is None) or ("UXNATIVE" is ""): # this should never happen
            # need to fill this with error message
            d["datatype"] = str("DEBUG_PROBLEM_HERE" + "/SafeHeadingAction")
            d["datastring"] = str(m)
        else:
            d['datatype'] = str("UXNATIVE" + "/SafeHeadingAction")
            d['datastring'] = str(m)
        return d

    def toDictMembers(self, d):
        VehicleAction.VehicleAction.toDictMembers(self, d)
        d['VehicleID'] = self.VehicleID
        d['OperatingRegion'] = self.OperatingRegion
        d['LeadAheadDistance'] = self.LeadAheadDistance
        d['LoiterRadius'] = self.LoiterRadius
        d['DesiredHeading'] = self.DesiredHeading
        d['DesiredHeadingRate'] = self.DesiredHeadingRate
        d['UseHeadingRate'] = self.UseHeadingRate

        return

    def getLMCPType(self):
        return self.LMCP_TYPE

    def getSeriesName(self):
        return self.SERIES_NAME

    def getSeriesNameID(self):
        return self.SERIES_NAME_ID

    def getSeriesVersion(self):
        return self.SERIES_VERSION

    def toXMLStr(self, ws):
        str = ws + '<SafeHeadingAction Series="UXNATIVE" >\n';
        #str +=VehicleAction.VehicleAction.toXMLMembersStr(self, ws + "  ")
        str += self.toXMLMembersStr(ws + "  ")
        str += ws + "</SafeHeadingAction>\n";
        return str

    def toXMLMembersStr(self, ws):
        buf = ""
        buf += VehicleAction.VehicleAction.toXMLMembersStr(self, ws)
        buf += ws + "<VehicleID>" + str(self.VehicleID) + "</VehicleID>\n"
        buf += ws + "<OperatingRegion>" + str(self.OperatingRegion) + "</OperatingRegion>\n"
        buf += ws + "<LeadAheadDistance>" + str(self.LeadAheadDistance) + "</LeadAheadDistance>\n"
        buf += ws + "<LoiterRadius>" + str(self.LoiterRadius) + "</LoiterRadius>\n"
        buf += ws + "<DesiredHeading>" + str(self.DesiredHeading) + "</DesiredHeading>\n"
        buf += ws + "<DesiredHeadingRate>" + str(self.DesiredHeadingRate) + "</DesiredHeadingRate>\n"
        buf += ws + "<UseHeadingRate>" + ('True' if self.UseHeadingRate else 'False') + "</UseHeadingRate>\n"

        return buf
        
