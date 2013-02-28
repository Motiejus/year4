#!/usr/bin/env python

import csv
import pickle

import numpy as np

d_gamma = 0.05
d_b = 10
d_a = 0.2
d_alpha = 0.9
d_sigma_sq = 1e-3
d_opts = d_gamma, d_b, d_a, d_alpha, d_sigma_sq

TRAINING = 'courseworkdata.csv'
TESTDATA = 'testdata.csv'
RESULTDATA = 'resultdata.csv'
LEARNING_ARR = 'learning.bin'


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


def learn_and_err(touches, targets, k, opts):
    """Learn and return sum root mean square of error

    Use [k .. k + 30] as test data"""
    touches_A = touches[:k] + touches[k + 30:]
    targets_A = targets[:k] + targets[k + 30:]

    touches_B = touches[k:k + 30]
    targets_B = targets[k:k + 30]

    l = learn(touches_A, targets_A, opts)
    error_sq = 0
    for i, s_star in enumerate(touches_B):
        s = predict(l, touches_A, s_star, opts)
        error = (s - targets_B[i])
        error_sq += np.dot(error.T, error)
    return error_sq / 30.0


def ten_fold_analysis(touches, targets, opts):
    """Operate on user-device, i.e. list of touches and targets"""
    error = []
    for i in range(10):
        error.append(learn_and_err(touches, targets, i * 30, opts))
    return np.average(error)


def main():
    with open(RESULTDATA, 'w') as w:
        do_full_prediction(w)


def get_learning_data():
    try:
        with open(LEARNING_ARR, 'r') as r:
            return pickle.load(r)
    except IOError:
        print ("'%s' not found! Creating. This will take a while..." %
                LEARNING_ARR)
    ret = do_full_learning()
    with open(LEARNING_ARR, 'w') as w:
        pickle.dump(ret, w)
    return ret


def do_full_prediction(res):
    learning = get_learning_data()

    fieldnames = ['sessionID', 'targetX', 'targetY', 'touchX',
            'touchY', 'name', 'phone']
    res.writelines([",".join(fieldnames), '\n'])

    writer = csv.DictWriter(res, fieldnames)
    with open(TESTDATA, 'r') as f:
        for l in csv.DictReader(f):
            subj, n = line_to_np(l, ['touchX', 'touchY'])
            x, y = predict(learning[subj]['learn'], learning[subj]['touches'],
                    n, d_opts)
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


def training_b():
    """Train b using 10-fold validation"""
    touches = read_file(TRAINING, ['touchX', 'touchY'])['15-Iphone4']
    targets = read_file(TRAINING, ['targetX', 'targetY'])['15-Iphone4']

    for b in np.arange(3, 10.1, 1):
        opts = list(d_opts)
        opts[1] = b
        err = ten_fold_analysis(touches, targets, opts)
        print("b=%.4f, error: %.7f" % (b, err))

if __name__ == '__main__':
    main()
