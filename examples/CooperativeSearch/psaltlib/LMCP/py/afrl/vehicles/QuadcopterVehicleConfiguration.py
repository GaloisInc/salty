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

from afrl.cmasi import EntityConfiguration


class QuadcopterVehicleConfiguration(EntityConfiguration.EntityConfiguration):

    def __init__(self):
        EntityConfiguration.EntityConfiguration.__init__(self)
        self.LMCP_TYPE = 7
        self.SERIES_NAME = "VEHICLES"
        self.FULL_LMCP_TYPE_NAME = "afrl.vehicles.QuadcopterVehicleConfiguration"
        #Series Name turned into a long for quick comparisons.
        self.SERIES_NAME_ID = 6216454340153722195
        self.SERIES_VERSION = 1

        #Define message fields
        self.MaximumAltitude = 0   #real32
        self.MaximumWindspeed = 0   #real32
        self.NumberOfRotors = 0   #int16


    def pack(self):
        """
        Packs the object data and returns a string that contains all of the serialized
        members.
        """
        buffer = []
        buffer.extend(EntityConfiguration.EntityConfiguration.pack(self))
        buffer.append(struct.pack(">f", self.MaximumAltitude))
        buffer.append(struct.pack(">f", self.MaximumWindspeed))
        buffer.append(struct.pack(">h", self.NumberOfRotors))

        return "".join(buffer)

    def unpack(self, buffer, _pos):
        """
        Unpacks data from a string buffer and sets class members
        """
        _pos = EntityConfiguration.EntityConfiguration.unpack(self, buffer, _pos)
        self.MaximumAltitude = struct.unpack_from(">f", buffer, _pos)[0]
        _pos += 4
        self.MaximumWindspeed = struct.unpack_from(">f", buffer, _pos)[0]
        _pos += 4
        self.NumberOfRotors = struct.unpack_from(">h", buffer, _pos)[0]
        _pos += 2
        return _pos


    def unpackFromXMLNode(self, el, seriesFactory):
        EntityConfiguration.EntityConfiguration.unpackFromXMLNode(self, el, seriesFactory)
        for e in el.childNodes:
            if e.nodeType == xml.dom.Node.ELEMENT_NODE:
                if e.localName == "MaximumAltitude" and len(e.childNodes) > 0 :
                    self.MaximumAltitude = float(e.childNodes[0].nodeValue)
                elif e.localName == "MaximumWindspeed" and len(e.childNodes) > 0 :
                    self.MaximumWindspeed = float(e.childNodes[0].nodeValue)
                elif e.localName == "NumberOfRotors" and len(e.childNodes) > 0 :
                    self.NumberOfRotors = int(e.childNodes[0].nodeValue)

        return

    def unpackFromDict(self, d, seriesFactory):
        EntityConfiguration.EntityConfiguration.unpackFromDict(self, d, seriesFactory)
        for key in d:
            if key == "MaximumAltitude":
                self.MaximumAltitude = d[key]
            elif key == "MaximumWindspeed":
                self.MaximumWindspeed = d[key]
            elif key == "NumberOfRotors":
                self.NumberOfRotors = d[key]

        return

    def get_MaximumAltitude(self):
        return self.MaximumAltitude

    def set_MaximumAltitude(self, value):
        self.MaximumAltitude = float( value )

    def get_MaximumWindspeed(self):
        return self.MaximumWindspeed

    def set_MaximumWindspeed(self, value):
        self.MaximumWindspeed = float( value )

    def get_NumberOfRotors(self):
        return self.NumberOfRotors

    def set_NumberOfRotors(self, value):
        self.NumberOfRotors = int( value )



    def toString(self):
        """
        Returns a string representation of all variables
        """
        buf = EntityConfiguration.EntityConfiguration.toString(self)
        buf += "From QuadcopterVehicleConfiguration:\n"
        buf +=    "MaximumAltitude = " + str( self.MaximumAltitude ) + "\n" 
        buf +=    "MaximumWindspeed = " + str( self.MaximumWindspeed ) + "\n" 
        buf +=    "NumberOfRotors = " + str( self.NumberOfRotors ) + "\n" 

        return buf;

    def toDict(self):
        m = {}
        self.toDictMembers(m)
        d = {}
        if ("VEHICLES" is None) or ("VEHICLES" is ""): # this should never happen
            # need to fill this with error message
            d["datatype"] = str("DEBUG_PROBLEM_HERE" + "/QuadcopterVehicleConfiguration")
            d["datastring"] = str(m)
        else:
            d['datatype'] = str("VEHICLES" + "/QuadcopterVehicleConfiguration")
            d['datastring'] = str(m)
        return d

    def toDictMembers(self, d):
        EntityConfiguration.EntityConfiguration.toDictMembers(self, d)
        d['MaximumAltitude'] = self.MaximumAltitude
        d['MaximumWindspeed'] = self.MaximumWindspeed
        d['NumberOfRotors'] = self.NumberOfRotors

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
        str = ws + '<QuadcopterVehicleConfiguration Series="VEHICLES" >\n';
        #str +=EntityConfiguration.EntityConfiguration.toXMLMembersStr(self, ws + "  ")
        str += self.toXMLMembersStr(ws + "  ")
        str += ws + "</QuadcopterVehicleConfiguration>\n";
        return str

    def toXMLMembersStr(self, ws):
        buf = ""
        buf += EntityConfiguration.EntityConfiguration.toXMLMembersStr(self, ws)
        buf += ws + "<MaximumAltitude>" + str(self.MaximumAltitude) + "</MaximumAltitude>\n"
        buf += ws + "<MaximumWindspeed>" + str(self.MaximumWindspeed) + "</MaximumWindspeed>\n"
        buf += ws + "<NumberOfRotors>" + str(self.NumberOfRotors) + "</NumberOfRotors>\n"

        return buf
        
