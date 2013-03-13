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
        t.getNode(i).bootAtTime((31 + t.ticksPerSecond() / 10) * i + 1)

    [t.runNextEvent() for i in range(60)]
    for i in range(0, 3):
        assert t.getNode(i).isOn(), "m%d is on" % i

def tick(ntimes):
    [t.runNextEvent() for i in range(ntimes)]


def inject_message(t, src, dst, action, data):
    msg = TinyBlogMsg()
    msg.set_data(data)
    msg.set_nchars(len(data))
    msg.set_sourceMoteID(src)
    msg.set_destMoteID(dst)
    msg.set_action(action)
    pkt = t.newPacket()

    pkt.setData(msg.data)
    pkt.setType(msg.get_amType())
    pkt.setSource(src)
    pkt.setDestination(dst)
    pkt.deliverNow(dst)


def num_followers(m1):
    d = m1.getVariable("TinyBlogC.num_followers").getData()
    try:
        return ord(d[0])
    except TypeError:
        return d


def scenario_1(t):
    """
    m0 is kind of a base station.

    m1 adds m2
    m2 tweets twice
    m0 asks m1 GET_TWEETS
        m1 replies with two tweets

    How we test it
    1. assert m1.num_followers = 0
    2. inject "ADD_USER 2" to m1 (as if it originated from m0)
    3. wait a while
    4. assert m2.followers == 1
    """
    start_nodes(t)
    m1 = t.getNode(1)
    assert num_followers(m1) == 0

    tick(10)
    inject_message(t, 0, 1, Action.ADD_USER, [2])
    tick(10)

    assert num_followers(m1) == 1
    """
    5. inject "POST_TWEET" to m2 (from m0) twice
    6. inject "GET_TWEETS" to m1 (from m0)
    7. Observe two tweets arriving from m1 to m0 (manually)
    """
    inject_message(t, 2, 1, Action.POST_TWEET, map(ord, "aaaa\04"))
    tick(10)
    inject_message(t, 2, 1, Action.POST_TWEET, map(ord, "bbbb\04"))
    tick(10)

    inject_message(t, 0, 1, Action.GET_TWEETS, [])
    tick(200)


if __name__ == '__main__':
    t = Tossim(TossimApp.NescApp().variables.variables())
    t.addChannel("All", sys.stdout)
    scenario_1(t)
