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
    data1 = execute(filename, "emz")
    data2 = execute(filename, "ez")

    matplotlib.use('Agg')
    bar = produce_bar(data1)
    bar.savefig('./res/predictions.pdf')

    table1 = produce_table(data1, "emz")
    table2 = produce_table(data2, "ez")
    correlations = produce_correlations(workable)
    with open('./res/report_data.tex', 'w') as f:
        f.write(table1)
        f.write("\n")
        f.write(table2)
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


def produce_table(data, suffix):
    data2 = {'silence': aggregate(data, 'silence'),
            'speech': aggregate(data, 'speech')}
    def fmt(c, b):
        if len(data2[c][b]):
            avg, std = np.average(data2[c][b]), np.std(data2[c][b])
            return "%.5f" % avg, "%.5f" % std
        else:
            return "   -   ", "   -   "

    corr_data = (
            "\\newcommand{\\sicorrectnumSUFFIX}{%d}\n"
            "\\newcommand{\\spcorrectnumSUFFIX}{%d}\n"
            "\\newcommand{\\siincorrectnumSUFFIX}{%d}\n"
            "\\newcommand{\\spincorrectnumSUFFIX}{%d}\n"

            "\\newcommand{\\sicorrectmeanSUFFIX}{%s}\n"
            "\\newcommand{\\sicorrectstddevSUFFIX}{%s}\n"
            "\\newcommand{\\spcorrectmeanSUFFIX}{%s}\n"
            "\\newcommand{\\spcorrectstddevSUFFIX}{%s}\n"
            "\\newcommand{\\siincorrectmeanSUFFIX}{%s}\n"
            "\\newcommand{\\siincorrectstddevSUFFIX}{%s}\n"
            "\\newcommand{\\spincorrectmeanSUFFIX}{%s}\n"
            "\\newcommand{\\spincorrectstddevSUFFIX}{%s}\n\n") % \
                    ((
                        len(data2['silence']['ok']),
                        len(data2['speech']['ok']),
                        len(data2['silence']['err']),
                        len(data2['speech']['err'])
                        ) + fmt('silence', 'ok') + \
                    fmt('speech', 'ok') + \
                    fmt('silence', 'err') + fmt('speech', 'err'))

    corr_table = ("\\begin{tabular}{r | c | c | c || c | c | c |}\n"
            "\\cline{2-7}\n"
            "& \\multicolumn{3}{|c||}{silence} "
            "& \\multicolumn{3}{c|}{speech}\\\\ \\cline{2-7}\n"
            "& len & mean & stddev & len & mean & stddev \\\\ \\hline\n"
            "correct classifications "
            "& $\\sicorrectnumSUFFIX$ "
            "& $\\sicorrectmeanSUFFIX$ & $\\sicorrectstddevSUFFIX$ "
            "& $\\spcorrectnumSUFFIX$ "
            "& $\\spcorrectmeanSUFFIX$ & $\\spcorrectstddevSUFFIX$ "
            "\\\\ \\hline\n"
            "incorrect classifications "
            "& $\\siincorrectnumSUFFIX$ "
            "& $\\siincorrectmeanSUFFIX$ & $\\siincorrectstddevSUFFIX$ "
            "& $\\spincorrectnumSUFFIX$ "
            "& $\\spincorrectmeanSUFFIX$ & $\\spincorrectstddevSUFFIX$ "
            "\\\\ \\hline\n"
            "\\end{tabular}")
    corr_table = ("\\newcommand{\\correlationtableSUFFIX}{\n%s\n}" % corr_table)

    return (corr_data + corr_table).replace("SUFFIX", suffix)


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
