#!/usr/bin/env python
"""
Gaussian Process learner for touching.

If reading CSV does not work, see comment in function do_full_prediction.

Motiejus Jakstys, 1003704
2013-02-28

"""

import csv
import pickle
import StringIO

import numpy as np

d_gamma = 0.05
d_b = 10
d_a = 0.1
d_alpha = 0.9
d_sigma_sq = 1e-3
d_opts = d_gamma, d_b, d_a, d_alpha, d_sigma_sq

TRAINING = 'courseworkdata.csv'
TESTDATA = 'testdata.csv'
RESULTDATA = 'resultdata.csv'
#RESULTDATA = 'resultdata-%.2f-%d-%.1f-%.1f-%.3f.csv'
LEARNING_ARR = 'learning-%.2f-%d-%.1f-%.1f-%.3f.bin'

###############################################################################
### Algorithmic part
###############################################################################

def Cov(sn, sm, opts):
    def distance(sn, sm):
        return sum(np.square(np.subtract(sn, sm)))
    gamma, b, a, _, _ = opts
    "Covariance function"
    return b * np.add(
            a * np.dot(np.transpose(sn), sm),
            (1 - a) * np.exp(-gamma * distance(sn, sm))
            )


def get_z(targets):
    "[target, ...] -> 1D locations vector"
    N = len(targets)
    ret = np.zeros(N * 2)
    for i, (x, y) in enumerate(targets):
        ret[i] = x
        ret[i + N] = y
    return np.transpose([ret])


def get_C(touches, opts):
    "covariance for initial (touch) data"
    N = len(touches)
    ret = np.zeros(N * N).reshape(N, N)
    for i, sn in enumerate(touches):
        for j, sm in enumerate(touches):
            ret[i, j] = Cov(sn, sm, opts)
    return ret


def get_c(touches, s_star, opts):
    "covariance between s* and N (touch) input vectors in training set"
    N = len(touches)
    ret = np.zeros(N)
    for i, si in enumerate(touches):
        ret[i] = Cov(s_star, si, opts)
    return np.array([ret])


def get_x_hat(x, opts):
    _, _, _, alpha, _ = opts
    return np.concatenate((
        np.concatenate((x, alpha * x), axis=1),
        np.concatenate((alpha * x, x), axis=1)
        ))


def predict(learned, touches_A, s_star, opts):
    """Predict target from:
    1. learned (def learn)
    2. touches_A (touches training set)
    3. s_star (touch location we want to predict)

    Return (x, y) pair, which is the predicted target.
    """
    c_hat = get_x_hat(get_c(touches_A, s_star, opts), opts)
    return c_hat.dot(learned)


def learn(touches, targets, opts):
    """[C_hat + sigma_sq * I] <dot> z

    Part of the equation. After multiplying c_hat by resulting matrix,
    you get the prediction"""
    _, _, _, _, sigma_sq = opts
    C_hat = get_x_hat(get_C(touches, opts), opts)
    N, N = C_hat.shape
    z = get_z(targets)

    C_hat_with_noise = C_hat + np.identity(N) * sigma_sq
    return np.dot(np.linalg.inv(C_hat_with_noise), z)


###############################################################################
### Boring part
###############################################################################


def line_to_np(l, keys):
    subj = "%s-%s" % (l['name'].replace("Subject", ""), l['phone'])
    return subj, np.array([[float(l[i]) for i in keys]]).transpose()


def read_file(ifile, keys):
    "user-phone -> list(records of np arrays)"
    data = {}
    with open(ifile, 'r') as f:
        for l in csv.DictReader(f):
            subj, n = line_to_np(l, keys)
            if subj not in data:
                data[subj] = []
            data[subj].append(n)
    return data


def get_learning_data(opts):
    try:
        with open(LEARNING_ARR % opts, 'r') as r:
            return pickle.load(r)
    except IOError:
        print ("'%s' not found! Creating. This will take a while..." %
                LEARNING_ARR % opts)
    ret = do_full_learning()
    with open(LEARNING_ARR % opts, 'w') as w:
        pickle.dump(ret, w)
    return ret


def do_full_prediction(res, opts):
    learning = get_learning_data(opts)

    fieldnames = ['sessionID', 'targetX', 'targetY', 'touchX',
            'touchY', 'name', 'phone']
    res.writelines([",".join(fieldnames), '\n'])

    writer = csv.DictWriter(res, fieldnames)
    with open(TESTDATA, 'r') as f:
        contents = f.read()
        # if there are some problems with decoding, try removing this line:
        contents = contents.replace('\r', '\n')
        for l in csv.DictReader(StringIO.StringIO(contents)):
            subj, n = line_to_np(l, ['touchX', 'touchY'])
            x, y = predict(learning[subj]['learn'], learning[subj]['touches'],
                    n, opts)
            l['targetX'] = "%f" % x
            l['targetY'] = "%f" % y
            writer.writerow(l)


def do_full_learning():
    touches = read_file(TRAINING, ['touchX', 'touchY'])
    targets = read_file(TRAINING, ['targetX', 'targetY'])

    ret = {}
    for subj in touches.keys():
        print ("%s" % subj)
        l = learn(touches[subj], targets[subj], d_opts)
        ret[subj] = {}
        ret[subj]['learn'] = l
        ret[subj]['touches'] = touches[subj]
    return ret


def main():
    #for a in np.arange(0.02, 0.41, 0.02):
    #    opts = d_gamma, d_b, a, d_alpha, d_sigma_sq
    #    with open(RESULTDATA % opts, 'w') as w:
    #        do_full_prediction(w, opts)
    #for b in np.arange(4, 15, 1):
    #    opts = d_gamma, b, d_a, d_alpha, d_sigma_sq
    #    with open(RESULTDATA % opts, 'w') as w:
    #        do_full_prediction(w, opts)
    with open(RESULTDATA, 'w') as w:
        do_full_prediction(w, d_opts)


if __name__ == '__main__':
    main()
