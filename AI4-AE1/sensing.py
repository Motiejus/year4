#!/usr/bin/env python

from __future__ import division

import sys
import unittest
import itertools

from collections import deque
from math import log
import numpy as np

WINDOW_SIZE = 300

def energy(samples, window_sz):
    window = deque([0]*window_sz, window_sz)
    acc = 0
    for v in samples:
        acc -= window.popleft()**2
        window.append(v)
        acc += v**2
        yield acc


def magnitude(samples, window_sz):
    window = deque([0]*window_sz, window_sz)
    acc = 0
    for v in samples:
        acc -= abs(window.popleft())
        window.append(v)
        acc += abs(v)
        yield acc


def zcr(samples, window_sz):
    def signs(samples):
        samples = itertools.chain(samples)
        sign = lambda x: 1 if x >= 0 else 0
        # one value before current one (previous)
        prev = samples.next()
        yield 0
        for sample in samples:
            yield sign(prev) - sign(sample)
            prev = sample

    window = deque([0]*window_sz, window_sz)
    acc = 0 # accumulator
    for sign in signs(samples):
        acc -= window.popleft()
        window.append(abs(sign))
        acc += abs(sign)
        yield acc / (2.0 * window_sz)

def read_samples(filename):
    with open(filename, 'r') as f:
        samples = [int(line) for line in f]
    return samples


def normalize(l):
    if len(l) == 0:
        return []
    factor = max([abs(v) for v in l])
    return [v / factor for v in l]


def plot(filename, program, title, type):
    samples = read_samples(filename)

    import matplotlib
    matplotlib.use('Agg')
    import matplotlib.pyplot as plt
    fig = plt.figure()
    plt.plot(normalize(list(program(samples, WINDOW_SIZE))))
    plt.plot(normalize(samples))
    plt.xticks(np.arange(0,2401,300))
    plt.legend((title, 'Signal'))
    plt.xlabel("Sample #")
    fig.savefig('./res/sample_%s.pdf' % type)


def usage(cmd):
    print ("Usage: %s COMMAND FILENAME" % cmd)
    print ("")
    print ("    COMMAND := e | m | z | test | stats")
    print ("    in case of test, FILENAME is not required")


def avg(stuff):
    n = 0
    Sum = 0.0
    for v in stuff:
        Sum += v
        n += 1
    return float(Sum) / n


class EnergyTestCase(unittest.TestCase):
    def test_empty(self):
        self.assertEqual([0] * 100, list(energy([0]*100, 10)))

    def test_simple(self):
        samples = [10] + [0] * 99
        ret = list(energy(samples, 10))
        self.assertEqual(100, ret[0])
        self.assertEqual(100, ret[1])
        self.assertEqual(100, ret[9])
        self.assertEqual(0, ret[10])

    def test_complex(self):
        samples = [10, 1, 1] + [0] * 97
        ret = list(energy(samples, 10))
        self.assertEqual(100, ret[0])
        self.assertEqual(101, ret[1])
        self.assertEqual(102, ret[2])
        self.assertEqual(102, ret[9])
        self.assertEqual(2, ret[10])
        self.assertEqual(1, ret[11])
        self.assertEqual(0, ret[12])


class MagnitudeTestCase(unittest.TestCase):
    def test_empty(self):
        self.assertEqual([0] * 100, list(magnitude([0]*100, 10)))

    def test_simple(self):
        samples = [10] + [0] * 99
        ret = list(magnitude(samples, 10))
        self.assertEqual(10, ret[0])
        self.assertEqual(10, ret[1])
        self.assertEqual(10, ret[9])
        self.assertEqual(0, ret[10])

    def test_complex(self):
        samples = [10, 1, 1] + [0] * 97
        ret = list(magnitude(samples, 10))
        self.assertEqual(10, ret[0])
        self.assertEqual(11, ret[1])
        self.assertEqual(12, ret[2])
        self.assertEqual(12, ret[9])
        self.assertEqual(2, ret[10])
        self.assertEqual(1, ret[11])
        self.assertEqual(0, ret[12])


class ZcrTestCase(unittest.TestCase):
    def test_empty(self):
        samples = [0] * 100
        self.assertEqual([0.0] * 100, list(zcr(samples, 10)))

    def test_simple1(self):
        samples = [-1, 1, -1, 1]
        ret = list(zcr(samples, 1))
        self.assertEqual([0, 0.5, 0.5, 0.5], ret)

    def test_simple2(self):
        samples = [-1, 1, -1, 1]
        ret = list(zcr(samples, 2))
        self.assertEqual([0, 0.25, 0.25, 0.25], ret)

    def txest_simple4a(self):
        samples = [-1, 1, -1, 1, -1, 1]
        ret = list(zcr(samples, 4))
        self.assertEqual([0, 0.125, 0.25, 0.375, 0.5, 0.5], ret)

    def txest_simple4b(self):
        samples =  [-1, 1, -1, 1, -1, 1, 1, 1, 1, 1, 1]
        expected = [0, 0.125, 0.25, 0.375, 0.5, 0.5, 0.375, 0.25, 0.125, 0, 0]
        ret = list(zcr(samples, 4))
        self.assertEqual(expected, ret)


if __name__ == "__main__":
    if len(sys.argv) == 1:
        usage(sys.argv[0])
    else:
        if 'stats' in sys.argv[1]:
            e = log(avg(energy(read_samples(sys.argv[2]), WINDOW_SIZE)))
            m = log(avg(magnitude(read_samples(sys.argv[2]), WINDOW_SIZE)))
            z = log(avg(zcr(read_samples(sys.argv[2]), WINDOW_SIZE)))
            print ("%s\t%.6f\t%.6f\t%.6f" % (sys.argv[2], e, m, z))
        elif 'm' == sys.argv[1]:
            plot(sys.argv[2], magnitude, "Magnitude", "m")
        elif 'e' == sys.argv[1]:
            plot(sys.argv[2], energy, "Energy", "e")
        elif 'z' == sys.argv[1]:
            plot(sys.argv[2], zcr, "ZCR", "z")
        elif 'test' in sys.argv[1]:
            suite = unittest.TestLoader().loadTestsFromModule(
                    sys.modules[__name__])
            unittest.TextTestRunner(verbosity=2).run(suite)
        else:
            usage(sys.argv[0])
