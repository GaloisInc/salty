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



class ImpactHeartbeat(LMCPObject.LMCPObject):

    def __init__(self):

        self.LMCP_TYPE = 16
        self.SERIES_NAME = "IMPACT"
        self.FULL_LMCP_TYPE_NAME = "afrl.impact.ImpactHeartbeat"
        #Series Name turned into a long for quick comparisons.
        self.SERIES_NAME_ID = 5281966179208134656
        self.SERIES_VERSION = 13

        #Define message fields
        self.ComponentLabel = ""   #string
        self.HeartbeatTime = 0   #int64


    def pack(self):
        """
        Packs the object data and returns a string that contains all of the serialized
        members.
        """
        buffer = bytearray()
        buffer.extend(LMCPObject.LMCPObject.pack(self))
        buffer.extend(struct.pack(">H", len(self.ComponentLabel) ))
        if len(self.ComponentLabel) > 0:
            if (sys.version_info > (3, 0)):
                buffer.extend(struct.pack( repr(len(self.ComponentLabel)) + "s", bytearray(self.ComponentLabel,'ascii')))
            else:
                buffer.extend(struct.pack( repr(len(self.ComponentLabel)) + "s", self.ComponentLabel))
        buffer.extend(struct.pack(">q", self.HeartbeatTime))

        return buffer

    def unpack(self, buffer, _pos):
        """
        Unpacks data from a bytearray and sets class members
        """
        _pos = LMCPObject.LMCPObject.unpack(self, buffer, _pos)
        _strlen = struct.unpack_from(">H", buffer, _pos )[0]
        _pos += 2
        if _strlen > 0:
            self.ComponentLabel = struct.unpack_from( repr(_strlen) + "s", buffer, _pos )[0]
            _pos += _strlen
        else:
             self.ComponentLabel = ""
        self.HeartbeatTime = struct.unpack_from(">q", buffer, _pos)[0]
        _pos += 8
        return _pos


    def unpackFromXMLNode(self, el, seriesFactory):
        LMCPObject.LMCPObject.unpackFromXMLNode(self, el, seriesFactory)
        for e in el.childNodes:
            if e.nodeType == xml.dom.Node.ELEMENT_NODE:
                if e.localName == "ComponentLabel" and len(e.childNodes) > 0 :
                    self.ComponentLabel = str(e.childNodes[0].nodeValue)
                elif e.localName == "HeartbeatTime" and len(e.childNodes) > 0 :
                    self.HeartbeatTime = int(e.childNodes[0].nodeValue)

        return

    def unpackFromDict(self, d, seriesFactory):
        LMCPObject.LMCPObject.unpackFromDict(self, d, seriesFactory)
        for key in d:
            if key == "ComponentLabel":
                self.ComponentLabel = d[key]
            elif key == "HeartbeatTime":
                self.HeartbeatTime = d[key]

        return

    def get_ComponentLabel(self):
        return self.ComponentLabel

    def set_ComponentLabel(self, value):
        self.ComponentLabel = str( value )

    def get_HeartbeatTime(self):
        return self.HeartbeatTime

    def set_HeartbeatTime(self, value):
        self.HeartbeatTime = int( value )



    def toString(self):
        """
        Returns a string representation of all variables
        """
        buf = LMCPObject.LMCPObject.toString(self)
        buf += "From ImpactHeartbeat:\n"
        buf +=    "ComponentLabel = " + str( self.ComponentLabel ) + "\n" 
        buf +=    "HeartbeatTime = " + str( self.HeartbeatTime ) + "\n" 

        return buf;

    def toDict(self):
        m = {}
        self.toDictMembers(m)
        d = {}
        if ("IMPACT" is None) or ("IMPACT" is ""): # this should never happen
            # need to fill this with error message
            d["datatype"] = str("DEBUG_PROBLEM_HERE" + "/ImpactHeartbeat")
            d["datastring"] = str(m)
        else:
            d['datatype'] = str("IMPACT" + "/ImpactHeartbeat")
            d['datastring'] = str(m)
        return d

    def toDictMembers(self, d):
        LMCPObject.LMCPObject.toDictMembers(self, d)
        d['ComponentLabel'] = self.ComponentLabel
        d['HeartbeatTime'] = self.HeartbeatTime

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
        str = ws + '<ImpactHeartbeat Series="IMPACT" >\n';
        #str +=LMCPObject.LMCPObject.toXMLMembersStr(self, ws + "  ")
        str += self.toXMLMembersStr(ws + "  ")
        str += ws + "</ImpactHeartbeat>\n";
        return str

    def toXMLMembersStr(self, ws):
        buf = ""
        buf += LMCPObject.LMCPObject.toXMLMembersStr(self, ws)
        buf += ws + "<ComponentLabel>" + str(self.ComponentLabel) + "</ComponentLabel>\n"
        buf += ws + "<HeartbeatTime>" + str(self.HeartbeatTime) + "</HeartbeatTime>\n"

        return buf
        
