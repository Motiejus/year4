#!/usr/bin/env python
"""This file takes result data and prepares resources for the report"""

import sys
import os
import operator

import numpy as np
import matplotlib

from analyze import execute

def main(filename):
    """Output resources needed for the report.
    
    That is:
    1. Bar chart of correct/incorrect predictions for each class
    2. LaTeX table summarizing the prediction performance.

    Output:
        ./res/predictions.eps
        ./res/prediction_table.tex
    """
    data = execute(filename)

    try:
        os.mkdir("./res")
    except OSError:
        pass

    matplotlib.use('Agg')
    bar = produce_bar(data)
    bar.savefig('./res/predictions.eps')

    output = produce_table(data)
    with open('./res/prediction_table.tex', 'w') as f:
        f.write(output)

def produce_bar(data):
    silence = aggregate(data, 'silence')
    speech = aggregate(data, 'speech')
    err = len(silence['err']), len(speech['err'])
    ok = len(silence['ok']), len(speech['ok'])
    ind = np.arange(2)
    width = 0.8
    import matplotlib.pyplot as plt
    fig = plt.figure()
    p1 = plt.bar(ind, err, width, color='r', hatch='o')
    p2 = plt.bar(ind, ok, width, bottom=err, color='y', hatch='/')
    plt.xticks(ind+width/2., ('Silence', 'Speech'))
    plt.yticks(np.arange(0,61,5))
    plt.legend( (p1[0], p2[0]), ('Incorrect', 'Correct') )
    plt.grid(axis='y')
    return fig


def produce_table(data):
    data2 = {'silence': aggregate(data, 'silence'),
            'speech': aggregate(data, 'speech')}
    def fmt(c, b):
        if len(data2[c][b]):
            avg, std = np.average(data2[c][b]*100), np.std(data2[c][b]*100)
            return "%2.7f" % avg, "%2.7f" % std
        else:
            return "   -   ", "   -   "

    ret = """
                    speech              silence
              | mean       |  stddev        |   mean         | stddev
    correct   |  %s  |    %s    |    %s    |  %s
    incorrect |  %s  |    %s    |    %s    |  %s
    """ % (fmt('speech', 'ok') + fmt('silence', 'ok') +
            fmt('speech', 'err') + fmt('silence', 'err'))
    return ret


def aggregate(data, c):
    filtered = [d[c] for fn, d in data.items() if c in fn]
    ok = np.array(filter(lambda x: x >= 0.5, filtered))
    err = np.array(filter(lambda x: x < 0.5, filtered))
    return {'ok': ok, 'err': err}


if __name__ == '__main__':
    if len(sys.argv) != 2:
        sys.stderr.write("One argument required, usually samples/summary.txt\n")
        sys.exit(1)
    main(sys.argv[1])
