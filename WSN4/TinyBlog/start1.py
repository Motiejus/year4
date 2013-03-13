#!/usr/bin/python

import sys
from TOSSIM import *
import tinyos.tossim.TossimApp as TossimApp

import pdb

class Action:
    POST_TWEET = 1
    ADD_USER = 2
    GET_TWEETS = 3

def start_nodes(t):
    for i in range(0, 4):
        t.getNode(i).bootAtTime(i)
    [t.runNextEvent() for i in range(60)]
    for i in range(0, 4):
        assert t.getNode(i).isOn(), "m%d is on" % i

def scenario_1(t):
    """
    m1 is kind of a base station.

    m2 adds m3
    m3 tweets twice
    m1 asks m2 GET_TWEETS
        m2 replies with two tweets

    How we test it
    1. assert m2.num_followers = 0
    2. inject "ADD_USER 3" to m2 (as if it originated from m1)
    3. wait a while
    4. assert m2.followers == 1
    """
    start_nodes(t)
    m2 = t.getNode(2)
    assert m2.getVariable("TinyBlogC.num_followers").getData() == 0
    from TinyBlogMsg import TinyBlogMsg

    msg = TinyBlogMsg()
    msg.set_data([3])
    msg.set_destMoteID(2)
    msg.set_sourceMoteID(1)
    msg.set_action(Action.ADD_USER)
    pkt = t.newPacket()

    [t.runNextEvent() for i in range(60)]
    pkt.setData(msg.data)
    pkt.setType(msg.get_amType())
    pkt.setSource(1)
    pkt.setDestination(2)
    pkt.deliverNow(2)
    [t.runNextEvent() for i in range(100)]
    assert ord(m2.getVariable("TinyBlogC.num_followers").getData()) == 1

if __name__ == '__main__':
    t = Tossim(TossimApp.NescApp().variables.variables())
    t.addChannel("All", sys.stdout)
    scenario_1(t)
