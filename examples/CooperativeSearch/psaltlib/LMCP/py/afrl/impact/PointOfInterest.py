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

from afrl.cmasi import Location3D
from afrl.impact import AreaActionOptions


class PointOfInterest(LMCPObject.LMCPObject):

    def __init__(self):

        self.LMCP_TYPE = 22
        self.SERIES_NAME = "IMPACT"
        self.FULL_LMCP_TYPE_NAME = "afrl.impact.PointOfInterest"
        #Series Name turned into a long for quick comparisons.
        self.SERIES_NAME_ID = 5281966179208134656
        self.SERIES_VERSION = 13

        #Define message fields
        self.PointID = 0   #int64
        self.Location = Location3D.Location3D()   #Location3D
        self.PointAction = AreaActionOptions.AreaActionOptions.Created   #AreaActionOptions
        self.PointLabel = ""   #string
        self.BackgroundBehaviorPoint = False   #bool


    def pack(self):
        """
        Packs the object data and returns a string that contains all of the serialized
        members.
        """
        buffer = []
        buffer.extend(LMCPObject.LMCPObject.pack(self))
        buffer.append(struct.pack(">q", self.PointID))
        buffer.append(struct.pack("B", self.Location != None ))
        if self.Location != None:
            buffer.append(struct.pack(">q", self.Location.SERIES_NAME_ID))
            buffer.append(struct.pack(">I", self.Location.LMCP_TYPE))
            buffer.append(struct.pack(">H", self.Location.SERIES_VERSION))
            buffer.append(self.Location.pack())
        buffer.append(struct.pack(">i", self.PointAction))
        buffer.append(struct.pack(">H", len(self.PointLabel) ))
        if len(self.PointLabel) > 0:
            buffer.append(struct.pack( `len(self.PointLabel)` + "s", str(self.PointLabel)))
        boolChar = 1 if self.BackgroundBehaviorPoint == True else 0
        buffer.append(struct.pack(">B",boolChar))

        return "".join(buffer)

    def unpack(self, buffer, _pos):
        """
        Unpacks data from a string buffer and sets class members
        """
        _pos = LMCPObject.LMCPObject.unpack(self, buffer, _pos)
        self.PointID = struct.unpack_from(">q", buffer, _pos)[0]
        _pos += 8
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
        self.PointAction = struct.unpack_from(">i", buffer, _pos)[0]
        _pos += 4
        _strlen = struct.unpack_from(">H", buffer, _pos )[0]
        _pos += 2
        if _strlen > 0:
            self.PointLabel = struct.unpack_from( `_strlen` + "s", buffer, _pos )[0]
            _pos += _strlen
        else:
             self.PointLabel = ""
        boolChar = struct.unpack_from(">B", buffer, _pos)[0]
        self.BackgroundBehaviorPoint = True if boolChar == 1 else False
        _pos += 1
        return _pos


    def unpackFromXMLNode(self, el, seriesFactory):
        LMCPObject.LMCPObject.unpackFromXMLNode(self, el, seriesFactory)
        for e in el.childNodes:
            if e.nodeType == xml.dom.Node.ELEMENT_NODE:
                if e.localName == "PointID" and len(e.childNodes) > 0 :
                    self.PointID = int(e.childNodes[0].nodeValue)
                elif e.localName == "Location" and len(e.childNodes) > 0 :
                    for n in e.childNodes:
                        if n.nodeType == xml.dom.Node.ELEMENT_NODE:
                            self.Location = seriesFactory.createObjectByName(n.getAttribute('Series'), n.localName)
                            if self.Location != None:
                                self.Location.unpackFromXMLNode(n, seriesFactory)
                elif e.localName == "PointAction" and len(e.childNodes) > 0 :
                    self.PointAction = AreaActionOptions.get_AreaActionOptions_str(e.childNodes[0].nodeValue)
                elif e.localName == "PointLabel" and len(e.childNodes) > 0 :
                    self.PointLabel = str(e.childNodes[0].nodeValue)
                elif e.localName == "BackgroundBehaviorPoint" and len(e.childNodes) > 0 :
                    self.BackgroundBehaviorPoint = e.childNodes[0].nodeValue.lower() == 'true'

        return

    def unpackFromDict(self, d, seriesFactory):
        LMCPObject.LMCPObject.unpackFromDict(self, d, seriesFactory)
        for key in d:
            if key == "PointID":
                self.PointID = d[key]
            elif key == "Location":
                self.Location = seriesFactory.unpackFromDict(d[key])
            elif key == "PointAction":
                self.PointAction = d[key]
            elif key == "PointLabel":
                self.PointLabel = d[key]
            elif key == "BackgroundBehaviorPoint":
                self.BackgroundBehaviorPoint = d[key]

        return

    def get_PointID(self):
        return self.PointID

    def set_PointID(self, value):
        self.PointID = int( value )

    def get_Location(self):
        return self.Location

    def set_Location(self, value):
        self.Location = value 

    def get_PointAction(self):
        return self.PointAction

    def set_PointAction(self, value):
        self.PointAction = value 

    def get_PointLabel(self):
        return self.PointLabel

    def set_PointLabel(self, value):
        self.PointLabel = str( value )

    def get_BackgroundBehaviorPoint(self):
        return self.BackgroundBehaviorPoint

    def set_BackgroundBehaviorPoint(self, value):
        self.BackgroundBehaviorPoint = bool( value )



    def toString(self):
        """
        Returns a string representation of all variables
        """
        buf = LMCPObject.LMCPObject.toString(self)
        buf += "From PointOfInterest:\n"
        buf +=    "PointID = " + str( self.PointID ) + "\n" 
        buf +=    "Location = " + str( self.Location ) + "\n" 
        buf +=    "PointAction = " + str( self.PointAction ) + "\n" 
        buf +=    "PointLabel = " + str( self.PointLabel ) + "\n" 
        buf +=    "BackgroundBehaviorPoint = " + str( self.BackgroundBehaviorPoint ) + "\n" 

        return buf;

    def toDict(self):
        m = {}
        self.toDictMembers(m)
        d = {}
        if ("IMPACT" is None) or ("IMPACT" is ""): # this should never happen
            # need to fill this with error message
            d["datatype"] = str("DEBUG_PROBLEM_HERE" + "/PointOfInterest")
            d["datastring"] = str(m)
        else:
            d['datatype'] = str("IMPACT" + "/PointOfInterest")
            d['datastring'] = str(m)
        return d

    def toDictMembers(self, d):
        LMCPObject.LMCPObject.toDictMembers(self, d)
        d['PointID'] = self.PointID
        if self.Location == None:
            d['Location'] = None
        else:
            d['Location'] = self.Location.toDict()
        d['PointAction'] = self.PointAction
        d['PointLabel'] = self.PointLabel
        d['BackgroundBehaviorPoint'] = self.BackgroundBehaviorPoint

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
        str = ws + '<PointOfInterest Series="IMPACT" >\n';
        #str +=LMCPObject.LMCPObject.toXMLMembersStr(self, ws + "  ")
        str += self.toXMLMembersStr(ws + "  ")
        str += ws + "</PointOfInterest>\n";
        return str

    def toXMLMembersStr(self, ws):
        buf = ""
        buf += LMCPObject.LMCPObject.toXMLMembersStr(self, ws)
        buf += ws + "<PointID>" + str(self.PointID) + "</PointID>\n"
        buf += ws + "<Location>\n"
        if self.Location == None:
            buf += ws + "    <null/>\n"
        else:
            buf += ws + self.Location.toXMLStr(ws + "    ") 
        buf += ws + "</Location>\n"
        buf += ws + "<PointAction>" + AreaActionOptions.get_AreaActionOptions_int(self.PointAction) + "</PointAction>\n"
        buf += ws + "<PointLabel>" + str(self.PointLabel) + "</PointLabel>\n"
        buf += ws + "<BackgroundBehaviorPoint>" + ('True' if self.BackgroundBehaviorPoint else 'False') + "</BackgroundBehaviorPoint>\n"

        return buf
        
