#!/usr/bin/python

import os
import sys
import time
import struct

import TinyBlogMsg
from tinyos.message import MoteIF
#from tinyos.message.Message import *
#from tinyos.message.SerialPacket import *
#from tinyos.packet.Serial import Serial

class Action:
    POST_TWEET = 1
    ADD_USER = 2
    GET_TWEETS = 3


class DataLogger:
    def __init__(self, motestring, src):
        self._src = src
        self.mif = MoteIF.MoteIF()
        self.tos_source = self.mif.addSource(motestring)
        self.mif.addListener(self, TinyBlogMsg.TinyBlogMsg)

    def _boilerplate(self):
        smsg = TinyBlogMsg.TinyBlogMsg()
        smsg.set_seqno(0)
        smsg.set_sourceMoteID(self._src)
        smsg.set_destMoteID(0)
        smsg.set_hopCount(0)
        smsg.set_nchars(0)
        smsg.set_data("")
        smsg.set_mood(0)

    def receive(self, src, msg):
        if msg.get_amType() == TinyBlogMsg.AM_TYPE:
            print (msg)
            #m = TinyBlogMsg.TinyBlogMsg(msg.dataGet())
        sys.stdout.flush()

    def send_get_tweets(self):
        smsg = self._boilerplate()
        smsg.set_action(Action.GET_TWEETS)
        self.mif.sendMsg(self.tos_source, 0xFFFF, smsg.get_amType(), 0, smsg)

    def send_tweet(self):
        pass

    def main_loop(self):
        while 1:
            time.sleep(5)
            self.send_get_tweets()

def main():
    if '-h' in sys.argv or len(sys.argv) != 4:
        print ("Usage: %s sf@localhost:9002 HOST_MOTEID USER_MOTEID" % \
                sys.argv[0])
        sys.exit(1)
    dl = DataLogger(sys.argv[1], int(sys.argv[2]))
    dl.main_loop()

if __name__ == "__main__":
    main()
