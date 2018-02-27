#! /usr/bin/python

import struct
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



class OnboardProcessorIsAlive(LMCPObject.LMCPObject):

    def __init__(self):

        self.LMCP_TYPE = 12
        self.SERIES_NAME = "UXNATIVE"
        self.FULL_LMCP_TYPE_NAME = "uxas.messages.uxnative.OnboardProcessorIsAlive"
        #Series Name turned into a long for quick comparisons.
        self.SERIES_NAME_ID = 6149751333668345413
        self.SERIES_VERSION = 3

        #Define message fields
        self.VehicleID = 0   #int64
        self.TimeSent = 0   #int64


    def pack(self):
        """
        Packs the object data and returns a string that contains all of the serialized
        members.
        """
        buffer = []
        buffer.extend(LMCPObject.LMCPObject.pack(self))
        buffer.append(struct.pack(">q", self.VehicleID))
        buffer.append(struct.pack(">q", self.TimeSent))

        return "".join(buffer)

    def unpack(self, buffer, _pos):
        """
        Unpacks data from a string buffer and sets class members
        """
        _pos = LMCPObject.LMCPObject.unpack(self, buffer, _pos)
        self.VehicleID = struct.unpack_from(">q", buffer, _pos)[0]
        _pos += 8
        self.TimeSent = struct.unpack_from(">q", buffer, _pos)[0]
        _pos += 8
        return _pos


    def unpackFromXMLNode(self, el, seriesFactory):
        LMCPObject.LMCPObject.unpackFromXMLNode(self, el, seriesFactory)
        for e in el.childNodes:
            if e.nodeType == xml.dom.Node.ELEMENT_NODE:
                if e.localName == "VehicleID" and len(e.childNodes) > 0 :
                    self.VehicleID = int(e.childNodes[0].nodeValue)
                elif e.localName == "TimeSent" and len(e.childNodes) > 0 :
                    self.TimeSent = int(e.childNodes[0].nodeValue)

        return

    def unpackFromDict(self, d, seriesFactory):
        LMCPObject.LMCPObject.unpackFromDict(self, d, seriesFactory)
        for key in d:
            if key == "VehicleID":
                self.VehicleID = d[key]
            elif key == "TimeSent":
                self.TimeSent = d[key]

        return

    def get_VehicleID(self):
        return self.VehicleID

    def set_VehicleID(self, value):
        self.VehicleID = int( value )

    def get_TimeSent(self):
        return self.TimeSent

    def set_TimeSent(self, value):
        self.TimeSent = int( value )



    def toString(self):
        """
        Returns a string representation of all variables
        """
        buf = LMCPObject.LMCPObject.toString(self)
        buf += "From OnboardProcessorIsAlive:\n"
        buf +=    "VehicleID = " + str( self.VehicleID ) + "\n" 
        buf +=    "TimeSent = " + str( self.TimeSent ) + "\n" 

        return buf;

    def toDict(self):
        m = {}
        self.toDictMembers(m)
        d = {}
        if ("UXNATIVE" is None) or ("UXNATIVE" is ""): # this should never happen
            # need to fill this with error message
            d["datatype"] = str("DEBUG_PROBLEM_HERE" + "/OnboardProcessorIsAlive")
            d["datastring"] = str(m)
        else:
            d['datatype'] = str("UXNATIVE" + "/OnboardProcessorIsAlive")
            d['datastring'] = str(m)
        return d

    def toDictMembers(self, d):
        LMCPObject.LMCPObject.toDictMembers(self, d)
        d['VehicleID'] = self.VehicleID
        d['TimeSent'] = self.TimeSent

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
        str = ws + '<OnboardProcessorIsAlive Series="UXNATIVE" >\n';
        #str +=LMCPObject.LMCPObject.toXMLMembersStr(self, ws + "  ")
        str += self.toXMLMembersStr(ws + "  ")
        str += ws + "</OnboardProcessorIsAlive>\n";
        return str

    def toXMLMembersStr(self, ws):
        buf = ""
        buf += LMCPObject.LMCPObject.toXMLMembersStr(self, ws)
        buf += ws + "<VehicleID>" + str(self.VehicleID) + "</VehicleID>\n"
        buf += ws + "<TimeSent>" + str(self.TimeSent) + "</TimeSent>\n"

        return buf
        
