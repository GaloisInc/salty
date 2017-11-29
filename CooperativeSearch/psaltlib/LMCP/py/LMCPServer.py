import SocketServer, time, socket
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

myHost = ''
myPort = 11041

class LMCPHandler(SocketServer.StreamRequestHandler):
    def handle(self):
        self.factory = LMCPFactory.LMCPFactory()
        print "Client address: %s" % (self.client_address,)
        while True:
            try:
                data = [self.request.recv(LMCPFactory.HEADER_SIZE)]
                print "header size: %d" % (len(data[0]),) 
                size = LMCPFactory.getSize(data[0])
                print "object size: %d" % (size,)
                data.append(self.request.recv(size+4)) # compensate for checksum
                data_str = "".join(data)
                print "%d bytes received" % (len(data_str),)
                recv_obj = self.factory.getObject(data_str)
                print "%s received" % recv_obj.__class__
                if recv_obj != None:
                    print "Printing object XML..."
                    print recv_obj.toXMLStr("")
                else:
                    print "Invalid object received."
            except socket.error, msg:
                print msg
                self.stop = True
        self.request.close()

if __name__ == '__main__':
    # make a threaded server, listen/handle clients forever
    myaddr = (myHost, myPort)
    server = SocketServer.TCPServer(myaddr, LMCPHandler)
    server.serve_forever()



    


