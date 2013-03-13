#!/usr/bin/python

import sys
from TOSSIM import *
from TinyBlogMsg import TinyBlogMsg

import tinyos.tossim.TossimApp as TossimApp

import pdb

class Action:
    POST_TWEET = 1
    ADD_USER = 2
    GET_TWEETS = 3

def start_nodes(t):
    for i in range(0, 3):
        t.getNode(i).bootAtTime(i)
    [t.runNextEvent() for i in range(60)]
    for i in range(0, 3):
        assert t.getNode(i).isOn(), "m%d is on" % i

def tick(ntimes):
    [t.runNextEvent() for i in range(ntimes)]


def inject_add_user(t):
    msg = TinyBlogMsg()
    msg.set_data([3])
    msg.set_destMoteID(1)
    msg.set_sourceMoteID(0)
    msg.set_action(Action.ADD_USER)
    pkt = t.newPacket()

    pkt.setData(msg.data)
    pkt.setType(msg.get_amType())
    pkt.setSource(0)
    pkt.setDestination(1)
    pkt.deliverNow(1)


def inject_post_tweet(t):
    msg = TinyBlogMsg()
    msg.set_data([3])
    msg.set_destMoteID(2)
    msg.set_sourceMoteID(1)
    msg.set_action(Action.POST_TWEET)
    pkt = t.newPacket()
    pkt.setData(msg.data)
    pkt.setType(msg.get_amType())
    pkt.setSource(0)
    pkt.setDestination(2)
    pkt.deliverNow(2)



def scenario_1(t):
    """
    m0 is kind of a base station.

    m1 adds m2
    m2 tweets twice
    m0 asks m1 GET_TWEETS
        m1 replies with two tweets

    How we test it
    1. assert m1.num_followers = 0
    2. inject "ADD_USER 3" to m2 (as if it originated from m1)
    3. wait a while
    4. assert m2.followers == 1
    """
    start_nodes(t)
    m1 = t.getNode(1)
    assert m1.getVariable("TinyBlogC.num_followers").getData() == 0

    tick(10)
    inject_add_user(t)
    tick(10)

    assert m1.getVariable("TinyBlogC.num_followers").getData() == 1

    """
    5. inject "POST_TWEET" to m3 (from m1) twice
    6. inject "GET_TWEETS" to m3 (from m1)
    7. Observe two tweets arriving from m3 to m1 (manually)
    """
    inject_post_tweet(t)
    tick(10)
    inject_post_tweet(t)
    tick(10)


if __name__ == '__main__':
    t = Tossim(TossimApp.NescApp().variables.variables())
    t.addChannel("All", sys.stdout)
    scenario_1(t)
