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



class TaskActive(LMCPObject.LMCPObject):

    def __init__(self):

        self.LMCP_TYPE = 27
        self.SERIES_NAME = "UXTASK"
        self.FULL_LMCP_TYPE_NAME = "uxas.messages.task.TaskActive"
        #Series Name turned into a long for quick comparisons.
        self.SERIES_NAME_ID = 6149757930721443840
        self.SERIES_VERSION = 7

        #Define message fields
        self.TaskID = 0   #int64
        self.EntityID = 0   #int64
        self.TimeTaskActivated = 0   #int64


    def pack(self):
        """
        Packs the object data and returns a string that contains all of the serialized
        members.
        """
        buffer = bytearray()
        buffer.extend(LMCPObject.LMCPObject.pack(self))
        buffer.extend(struct.pack(">q", self.TaskID))
        buffer.extend(struct.pack(">q", self.EntityID))
        buffer.extend(struct.pack(">q", self.TimeTaskActivated))

        return buffer

    def unpack(self, buffer, _pos):
        """
        Unpacks data from a bytearray and sets class members
        """
        _pos = LMCPObject.LMCPObject.unpack(self, buffer, _pos)
        self.TaskID = struct.unpack_from(">q", buffer, _pos)[0]
        _pos += 8
        self.EntityID = struct.unpack_from(">q", buffer, _pos)[0]
        _pos += 8
        self.TimeTaskActivated = struct.unpack_from(">q", buffer, _pos)[0]
        _pos += 8
        return _pos


    def unpackFromXMLNode(self, el, seriesFactory):
        LMCPObject.LMCPObject.unpackFromXMLNode(self, el, seriesFactory)
        for e in el.childNodes:
            if e.nodeType == xml.dom.Node.ELEMENT_NODE:
                if e.localName == "TaskID" and len(e.childNodes) > 0 :
                    self.TaskID = int(e.childNodes[0].nodeValue)
                elif e.localName == "EntityID" and len(e.childNodes) > 0 :
                    self.EntityID = int(e.childNodes[0].nodeValue)
                elif e.localName == "TimeTaskActivated" and len(e.childNodes) > 0 :
                    self.TimeTaskActivated = int(e.childNodes[0].nodeValue)

        return

    def unpackFromDict(self, d, seriesFactory):
        LMCPObject.LMCPObject.unpackFromDict(self, d, seriesFactory)
        for key in d:
            if key == "TaskID":
                self.TaskID = d[key]
            elif key == "EntityID":
                self.EntityID = d[key]
            elif key == "TimeTaskActivated":
                self.TimeTaskActivated = d[key]

        return

    def get_TaskID(self):
        return self.TaskID

    def set_TaskID(self, value):
        self.TaskID = int( value )

    def get_EntityID(self):
        return self.EntityID

    def set_EntityID(self, value):
        self.EntityID = int( value )

    def get_TimeTaskActivated(self):
        return self.TimeTaskActivated

    def set_TimeTaskActivated(self, value):
        self.TimeTaskActivated = int( value )



    def toString(self):
        """
        Returns a string representation of all variables
        """
        buf = LMCPObject.LMCPObject.toString(self)
        buf += "From TaskActive:\n"
        buf +=    "TaskID = " + str( self.TaskID ) + "\n" 
        buf +=    "EntityID = " + str( self.EntityID ) + "\n" 
        buf +=    "TimeTaskActivated = " + str( self.TimeTaskActivated ) + "\n" 

        return buf;

    def toDict(self):
        m = {}
        self.toDictMembers(m)
        d = {}
        if ("UXTASK" is None) or ("UXTASK" is ""): # this should never happen
            # need to fill this with error message
            d["datatype"] = str("DEBUG_PROBLEM_HERE" + "/TaskActive")
            d["datastring"] = str(m)
        else:
            d['datatype'] = str("UXTASK" + "/TaskActive")
            d['datastring'] = str(m)
        return d

    def toDictMembers(self, d):
        LMCPObject.LMCPObject.toDictMembers(self, d)
        d['TaskID'] = self.TaskID
        d['EntityID'] = self.EntityID
        d['TimeTaskActivated'] = self.TimeTaskActivated

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
        str = ws + '<TaskActive Series="UXTASK" >\n';
        #str +=LMCPObject.LMCPObject.toXMLMembersStr(self, ws + "  ")
        str += self.toXMLMembersStr(ws + "  ")
        str += ws + "</TaskActive>\n";
        return str

    def toXMLMembersStr(self, ws):
        buf = ""
        buf += LMCPObject.LMCPObject.toXMLMembersStr(self, ws)
        buf += ws + "<TaskID>" + str(self.TaskID) + "</TaskID>\n"
        buf += ws + "<EntityID>" + str(self.EntityID) + "</EntityID>\n"
        buf += ws + "<TimeTaskActivated>" + str(self.TimeTaskActivated) + "</TimeTaskActivated>\n"

        return buf
        
