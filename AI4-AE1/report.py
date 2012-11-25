#!/usr/bin/env python
"""This file takes result data and prepares resources for the report"""

import sys
import os
import operator

from scipy.stats.stats import pearsonr
import numpy as np
import matplotlib

from analyze import read, execute

def main(filename):
    """Output resources needed for the report.
    
    That is:
    1. Bar chart of correct/incorrect predictions for each class
    2. LaTeX table summarizing the prediction performance.

    Output:
        ./res/predictions.pdf
        ./res/report_data.tex
    """
    workable = read(filename)
    data = execute(filename)

    matplotlib.use('Agg')
    bar = produce_bar(data)
    bar.savefig('./res/predictions.pdf')

    table = produce_table(data)
    correlations = produce_correlations(workable)
    with open('./res/report_data.tex', 'w') as f:
        f.write(table)
        f.write("\n\n")
        f.write(correlations)


def produce_correlations(w):
    e = [d['e'] for d in w['silence']] + [d['e'] for d in w['speech']]
    m = [d['m'] for d in w['silence']] + [d['m'] for d in w['speech']]
    z = [d['z'] for d in w['silence']] + [d['z'] for d in w['speech']]
    return (
            "\\newcommand{\\correlationem}{%.4f}\n"
            "\\newcommand{\\correlationez}{%.4f}\n"
            "\\newcommand{\\correlationmz}{%.4f}\n") % \
                    (pearsonr(e, m)[0], pearsonr(e, z)[0], pearsonr(m, z)[0])



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
    plt.legend((p1[0], p2[0]), ('Incorrect', 'Correct') )
    plt.grid(axis='y')
    return fig


def produce_table(data):
    data2 = {'silence': aggregate(data, 'silence'),
            'speech': aggregate(data, 'speech')}
    def fmt(c, b):
        if len(data2[c][b]):
            avg, std = np.average(data2[c][b]), np.std(data2[c][b])
            return "%.5f" % avg, "%.5f" % std
        else:
            return "   -   ", "   -   "

    corr_data = (
            "\\newcommand{\\sicorrectmean}{%s}\n"
            "\\newcommand{\\sicorrectstddev}{%s}\n"
            "\\newcommand{\\spcorrectmean}{%s}\n"
            "\\newcommand{\\spcorrectstddev}{%s}\n"
            "\\newcommand{\\siincorrectmean}{%s}\n"
            "\\newcommand{\\siincorrectstddev}{%s}\n"
            "\\newcommand{\\spincorrectmean}{%s}\n"
            "\\newcommand{\\spincorrectstddev}{%s}\n\n") % \
                    (fmt('speech', 'ok') + fmt('silence', 'ok') +
                            fmt('speech', 'err') + fmt('silence', 'err'))

    corr_table = ("\\begin{tabular}{r | c | c || c | c |}\n"
            "\\cline{2-5}\n"
            "& \\multicolumn{2}{|c||}{silence} "
            "& \\multicolumn{2}{c|}{speech}\\\\ \\cline{2-5}\n"
            "& mean & stddev & mean & stddev \\\\ \\hline\n"
            "correct samples "
            "& $\\sicorrectmean$ & $\\sicorrectstddev$ "
            "& $\\spcorrectmean$ & $\\spcorrectstddev$ \\\\ \\hline\n"
            "incorrect samples "
            "& $\\siincorrectmean$ & $\\siincorrectstddev$ "
            "& $\\spincorrectmean$ & $\\spincorrectstddev$ \\\\ \\hline\n"
            "\\end{tabular}")
    return corr_data + ("\\newcommand{\\correlationtable}{\n%s\n}" % corr_table)


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
