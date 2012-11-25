#!/usr/bin/env python
"""
This file takes raw data (averages of Energy, Magnitude, ZCR), does the
10-fold training (10 trainings and executions in total), and outputs the
probability of silence/speech for every file
"""

import sys

import numpy as np


def main(filename):
    """Print raw training results:

        Filename, % silence, % speech
    """
    data = execute(sys.argv[1], "emz")
    ret = [(fn, d['silence'], d['speech']) for fn, d in data.items()]
    for item in sorted(ret):
        print ("%s\t%f\t%f" % item)


def execute(filename, classes):
    """Do the training + estimation. Return probabilities of silence and speech.
    
    Output format:
    {
        FILENAME: {"silence": float, "speech": float},
        ...
    }
    """
    ret = {}
    workable = read(filename)
    for lower in range(0, 50, 5):
        training_set, test_set = {}, []
        for category, items in workable.items():
            test_set += items[lower:lower+5]
            training_set[category] = items[:lower] + items[lower+5:]
        for item in test_set:
            ret[item['_fn']] = posterior(training_set, item, classes)
    return ret


def read(filename):
    """Read data from summary.txt and return a workable array
    
    Input file must consist of the following lines:

    FILENAME    ENERGY  MAGNITUDE   ZCR

    Where
        FILENAME = where the sample was acquired.
                   Must start with "silence_" or "speech_"
                   so class can be distinguished.
        ENERGY, MAGNITUDE, ZCR = floats. Mean measurement in the file.
    """
    ret = {'silence' : [], 'speech': []}
    with open(filename, 'r') as f:
        for line in f:
            (label, es, ms, zs) = line.split()
            (e, m, z) = map(float, [es, ms, zs])
            category = label[:label.find('_')]
            ret[category].append({'_fn': label, 'e': e, 'm': m, 'z': z})
    return ret


def posterior(workable, measurement, classes):
    """Return posterior probability of given measurement with learned stats

    workable is the array returned by read/1
    measurement = {"e": float, "m": float, "z": float}
    In other words, measurement is the single element of "workable" array

    Return:

        {"silence": float, "speech": float}
        Where silence + float add up to 1 (normalized).

    Assume probabilities for speech and silence are 0.5"""
    stats = make_stats(workable, classes)
    ret = {}
    for category, items in stats.items():
        p = 0.5 # prior probability of getting into this class
        for stat, d in items.items(): # stat = e|m|z, d={"avg": ..., }
            p *= prob(measurement[stat], d['avg'], d['var'])
        ret[category] = p
    return normalize(ret)

### ----------------------------------------------------------------------------
### Helpers
### ----------------------------------------------------------------------------


def prob(value, avg, variance):
    "Probability distribution given value, average and variance"
    return 1/np.sqrt(2*np.pi*variance) * \
            np.exp( -(value - avg)**2/(2*variance) )


def normalize(d):
    "Normalize shallow dictionary"
    s = sum(d.values())
    for category in d.keys():
        d[category] /= s
    return d


def make_stats(workable, classes):
    """Take workable array (returned by read/1) and produce statistics

    Classes: list of classes which we want to take into account. Normally
    ['e', 'm', 'z'] or ['e', 'z']
    
    Output format:
        {
            "silence": {
                       "e": {"avg": float, "var": float},
                       "m": {"avg": float, "var": float},
                       "z": {"avg": float, "var": float}
                   },
            "speech": [...]
        }
    """
    ret = {}
    for category, measurements in workable.items():
        ret[category] = {}
        for stat in classes:
            if stat not in ret[category]:
                ret[category][stat] = {}
            acc = [measurement[stat] for measurement in measurements]
            ret[category][stat]["avg"] = np.average(acc)
            ret[category][stat]["var"] = np.var(acc)
    return ret


if __name__ == '__main__':
    if len(sys.argv) != 2:
        sys.stderr.write("One argument required, usually samples/summary.txt\n")
        sys.exit(1)
    main(sys.argv[1])
