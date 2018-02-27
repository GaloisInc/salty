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

from afrl.cmasi import Task
from afrl.cmasi import Location3D
from uxas.messages.task import PlanningState


class RendezvousTask(Task.Task):

    def __init__(self):
        Task.Task.__init__(self)
        self.LMCP_TYPE = 2
        self.SERIES_NAME = "UXTASK"
        self.FULL_LMCP_TYPE_NAME = "uxas.messages.task.RendezvousTask"
        #Series Name turned into a long for quick comparisons.
        self.SERIES_NAME_ID = 6149757930721443840
        self.SERIES_VERSION = 7

        #Define message fields
        self.NumberOfParticipants = 0   #byte
        self.Location = None   #Location3D
        self.Heading = 0   #real32
        self.MultiLocationRendezvous = False   #bool
        self.RendezvousStates = []   #PlanningState


    def pack(self):
        """
        Packs the object data and returns a string that contains all of the serialized
        members.
        """
        buffer = bytearray()
        buffer.extend(Task.Task.pack(self))
        buffer.extend(struct.pack(">B", self.NumberOfParticipants))
        buffer.extend(struct.pack("B", self.Location != None ))
        if self.Location != None:
            buffer.extend(struct.pack(">q", self.Location.SERIES_NAME_ID))
            buffer.extend(struct.pack(">I", self.Location.LMCP_TYPE))
            buffer.extend(struct.pack(">H", self.Location.SERIES_VERSION))
            buffer.extend(self.Location.pack())
        buffer.extend(struct.pack(">f", self.Heading))
        boolChar = 1 if self.MultiLocationRendezvous == True else 0
        buffer.extend(struct.pack(">B",boolChar))
        buffer.extend(struct.pack(">H", len(self.RendezvousStates) ))
        for x in self.RendezvousStates:
           buffer.extend(struct.pack("B", x != None ))
           if x != None:
               buffer.extend(struct.pack(">q", x.SERIES_NAME_ID))
               buffer.extend(struct.pack(">I", x.LMCP_TYPE))
               buffer.extend(struct.pack(">H", x.SERIES_VERSION))
               buffer.extend(x.pack())

        return buffer

    def unpack(self, buffer, _pos):
        """
        Unpacks data from a bytearray and sets class members
        """
        _pos = Task.Task.unpack(self, buffer, _pos)
        self.NumberOfParticipants = struct.unpack_from(">B", buffer, _pos)[0]
        _pos += 1
        _valid = struct.unpack_from("B", buffer, _pos )[0]
        _pos += 1
        if _valid:
            _series = struct.unpack_from(">q", buffer, _pos)[0]
            _pos += 8
            _type = struct.unpack_from(">I", buffer, _pos)[0]
            _pos += 4
            _version = struct.unpack_from(">H", buffer, _pos)[0]
            _pos += 2
            from lmcp import LMCPFactory
            self.Location = LMCPFactory.LMCPFactory().createObject(_series, _version, _type )
            _pos = self.Location.unpack(buffer, _pos)
        else:
            self.Location = None
        self.Heading = struct.unpack_from(">f", buffer, _pos)[0]
        _pos += 4
        boolChar = struct.unpack_from(">B", buffer, _pos)[0]
        self.MultiLocationRendezvous = True if boolChar == 1 else False
        _pos += 1
        _arraylen = struct.unpack_from(">H", buffer, _pos )[0]
        _pos += 2
        self.RendezvousStates = [None] * _arraylen
        for x in range(_arraylen):
            _valid = struct.unpack_from("B", buffer, _pos )[0]
            _pos += 1
            if _valid:
                _series = struct.unpack_from(">q", buffer, _pos)[0]
                _pos += 8
                _type = struct.unpack_from(">I", buffer, _pos)[0]
                _pos += 4
                _version = struct.unpack_from(">H", buffer, _pos)[0]
                _pos += 2
                from lmcp import LMCPFactory
                self.RendezvousStates[x] = LMCPFactory.LMCPFactory().createObject(_series, _version, _type )
                _pos = self.RendezvousStates[x].unpack(buffer, _pos)
            else:
                self.RendezvousStates[x] = None
        return _pos


    def unpackFromXMLNode(self, el, seriesFactory):
        Task.Task.unpackFromXMLNode(self, el, seriesFactory)
        for e in el.childNodes:
            if e.nodeType == xml.dom.Node.ELEMENT_NODE:
                if e.localName == "NumberOfParticipants" and len(e.childNodes) > 0 :
                    self.NumberOfParticipants = int(e.childNodes[0].nodeValue)
                elif e.localName == "Location" and len(e.childNodes) > 0 :
                    for n in e.childNodes:
                        if n.nodeType == xml.dom.Node.ELEMENT_NODE:
                            self.Location = seriesFactory.createObjectByName(n.getAttribute('Series'), n.localName)
                            if self.Location != None:
                                self.Location.unpackFromXMLNode(n, seriesFactory)
                elif e.localName == "Heading" and len(e.childNodes) > 0 :
                    self.Heading = float(e.childNodes[0].nodeValue)
                elif e.localName == "MultiLocationRendezvous" and len(e.childNodes) > 0 :
                    self.MultiLocationRendezvous = e.childNodes[0].nodeValue.lower() == 'true'
                elif e.localName == "RendezvousStates" and len(e.childNodes) > 0 :
                    self.RendezvousStates = []
                    for c in e.childNodes:
                        if c.nodeType == xml.dom.Node.ELEMENT_NODE:
                            obj = seriesFactory.createObjectByName(c.getAttribute('Series'), c.localName)
                            if obj != None:
                                obj.unpackFromXMLNode(c, seriesFactory)
                                self.RendezvousStates.append(obj)

        return

    def unpackFromDict(self, d, seriesFactory):
        Task.Task.unpackFromDict(self, d, seriesFactory)
        for key in d:
            if key == "NumberOfParticipants":
                self.NumberOfParticipants = d[key]
            elif key == "Location":
                self.Location = seriesFactory.unpackFromDict(d[key])
            elif key == "Heading":
                self.Heading = d[key]
            elif key == "MultiLocationRendezvous":
                self.MultiLocationRendezvous = d[key]
            elif key == "RendezvousStates":
                self.RendezvousStates = []
                for c in d[key]:
                    obj = seriesFactory.unpackFromDict(c)
                    if obj != None:
                        self.RendezvousStates.append(obj)

        return

    def get_NumberOfParticipants(self):
        return self.NumberOfParticipants

    def set_NumberOfParticipants(self, value):
        self.NumberOfParticipants = int( value )

    def get_Location(self):
        return self.Location

    def set_Location(self, value):
        self.Location = value 

    def get_Heading(self):
        return self.Heading

    def set_Heading(self, value):
        self.Heading = float( value )

    def get_MultiLocationRendezvous(self):
        return self.MultiLocationRendezvous

    def set_MultiLocationRendezvous(self, value):
        self.MultiLocationRendezvous = bool( value )

    def get_RendezvousStates(self):
        return self.RendezvousStates



    def toString(self):
        """
        Returns a string representation of all variables
        """
        buf = Task.Task.toString(self)
        buf += "From RendezvousTask:\n"
        buf +=    "NumberOfParticipants = " + str( self.NumberOfParticipants ) + "\n" 
        buf +=    "Location = " + str( self.Location ) + "\n" 
        buf +=    "Heading = " + str( self.Heading ) + "\n" 
        buf +=    "MultiLocationRendezvous = " + str( self.MultiLocationRendezvous ) + "\n" 
        buf +=    "RendezvousStates = " + str( self.RendezvousStates ) + "\n" 

        return buf;

    def toDict(self):
        m = {}
        self.toDictMembers(m)
        d = {}
        if ("UXTASK" is None) or ("UXTASK" is ""): # this should never happen
            # need to fill this with error message
            d["datatype"] = str("DEBUG_PROBLEM_HERE" + "/RendezvousTask")
            d["datastring"] = str(m)
        else:
            d['datatype'] = str("UXTASK" + "/RendezvousTask")
            d['datastring'] = str(m)
        return d

    def toDictMembers(self, d):
        Task.Task.toDictMembers(self, d)
        d['NumberOfParticipants'] = self.NumberOfParticipants
        if self.Location == None:
            d['Location'] = None
        else:
            d['Location'] = self.Location.toDict()
        d['Heading'] = self.Heading
        d['MultiLocationRendezvous'] = self.MultiLocationRendezvous
        d['RendezvousStates'] = []
        for x in self.RendezvousStates:
            if x == None:
                d['RendezvousStates'].append(None)
            else:
                d['RendezvousStates'].append(x.toDict())

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
        str = ws + '<RendezvousTask Series="UXTASK" >\n';
        #str +=Task.Task.toXMLMembersStr(self, ws + "  ")
        str += self.toXMLMembersStr(ws + "  ")
        str += ws + "</RendezvousTask>\n";
        return str

    def toXMLMembersStr(self, ws):
        buf = ""
        buf += Task.Task.toXMLMembersStr(self, ws)
        buf += ws + "<NumberOfParticipants>" + str(self.NumberOfParticipants) + "</NumberOfParticipants>\n"
        buf += ws + "<Location>\n"
        if self.Location == None:
            buf += ws + "    <null/>\n"
        else:
            buf += ws + self.Location.toXMLStr(ws + "    ") 
        buf += ws + "</Location>\n"
        buf += ws + "<Heading>" + str(self.Heading) + "</Heading>\n"
        buf += ws + "<MultiLocationRendezvous>" + ('True' if self.MultiLocationRendezvous else 'False') + "</MultiLocationRendezvous>\n"
        buf += ws + "<RendezvousStates>\n"
        for x in self.RendezvousStates:
            if x == None:
                buf += ws + "    <null/>\n"
            else:
                buf += x.toXMLStr(ws + "    ") 
        buf += ws + "</RendezvousStates>\n"

        return buf
        
