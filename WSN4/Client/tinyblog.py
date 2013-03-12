#!/usr/bin/python

from __future__ import print_function

import os
import sys
import time
import struct

import TinyBlogMsg
from tinyos.message import MoteIF
#from tinyos.message.Message import *
#from tinyos.message.SerialPacket import *
#from tinyos.packet.Serial import Serial

DATA_SIZE = int(os.getenv('DATA_SIZE', 14))

class Action:
    POST_TWEET = 1
    ADD_USER = 2
    GET_TWEETS = 3


class Tweeter:
    def __init__(self, motestring, src):
        self._src = src
        self.mif = MoteIF.MoteIF()
        self.tos_source = self.mif.addSource(motestring)

    def set_listener(self):
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
        return smsg

    def receive(self, src, msg):
        if msg.get_amType() == TinyBlogMsg.AM_TYPE:
            m = TinyBlogMsg.TinyBlogMsg(msg.dataGet())
            text = m.data[8:][:m.get_nchars()-1]
            print ("Tweet from '%d': '%s'" % (m.get_sourceMoteID(), text))
        sys.stdout.flush()


    def send_msg(self, msgtype, dst, msg=""):
        smsg = self._boilerplate()
        smsg.set_action(msgtype)
        smsg.set_nchars(len(msg))
        smsg.set_data(map(ord, msg))
        self.mif.sendMsg(self.tos_source, 0xFFFF, smsg.get_amType(), 0, smsg)


def main_loop(tweeter, user_moteid):
    while True:
        print ("\nEnter your command or tweet (/h for help):")
        line = raw_input('> ')
        if line.startswith('/h'):
            print(("  /follow NUMBER - express interest in SOMEBODY\n"
                "  /h               - this help\n"
                "  /q               - quit\n"
                "  ANYTHING ELSE    - tweet the message\n"))
        elif line.startswith('/follow '):
            obj = str(int(line.split(' ')[1]))
            print ("Following user %s" % obj)
            tweeter.send_msg(Action.ADD_USER, user_moteid, obj)
        elif line.startswith('/q'):
            sys.exit()
        else:
            msg = line[:DATA_SIZE-1].strip() + '\04'
            print ("Tweeting '%s'" % msg)
            tweeter.send_msg(Action.POST_TWEET, user_moteid, msg)

def main():
    if '-h' in sys.argv or len(sys.argv) != 4:
        print ("Usage: %s sf@localhost:9002 HOST_MOTEID USER_MOTEID" % \
                sys.argv[0])
        sys.exit(1)
    tw = Tweeter(sys.argv[1], int(sys.argv[2]))
    tw.set_listener()
    main_loop(tw, int(sys.argv[3]))

if __name__ == "__main__":
    main()
