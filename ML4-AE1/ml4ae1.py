#!/usr/bin/env python

import csv

import numpy as np

d_gamma = 0.05
d_b = 5
d_a = 0.1
d_alpha = 0.9
d_sigma_sq = 1e-3
d_opts = d_gamma, d_b, d_a, d_alpha, d_sigma_sq


def Cov(sn, sm, opts=d_opts):
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


def get_C(touches, opts=d_opts):
    "covariance for initial (touch) data"
    N = len(touches)
    ret = np.zeros(N * N).reshape(N, N)
    for i, sn in enumerate(touches):
        for j, sm in enumerate(touches):
            ret[i, j] = Cov(sn, sm, opts)
    return ret


def get_c(touches, s_star, opts=d_opts):
    gamma, b, a, alpha, sigma_sq = opts
    "covariance between s* and N (touch) input vectors in training set"
    N = len(touches)
    ret = np.zeros(N)
    for i, si in enumerate(touches):
        ret[i] = Cov(s_star, si, opts)
    return np.array([ret])


def get_x_hat(x, opts=d_opts):
    _, _, _, alpha, _ = opts
    return np.concatenate((
        np.concatenate((x, alpha * x), axis=1),
        np.concatenate((alpha * x, x), axis=1)
        ))


def predict(learned, touches_A, s_star):
    """Predict target from:
    1. learned (def learn)
    2. touches_A (touches training set)
    3. s_star (touch location we want to predict)

    Return (x, y) pair, which is the predicted target.
    """
    c_hat = get_x_hat(get_c(touches_A, s_star))
    return c_hat.dot(learned)


def learn(touches, targets, opts):
    """[C_hat + sigma_sq * I] <dot> z

    Part of the equation. After multiplying c_hat by resulting matrix,
    you get the prediction"""
    _, _, _, _, sigma_sq = opts
    C_hat = get_x_hat(get_C(touches), opts)
    N, N = C_hat.shape
    z = get_z(targets)

    C_hat_with_noise = C_hat + np.identity(N) * sigma_sq
    return np.dot(np.linalg.inv(C_hat_with_noise), z)


def read_file(ifile, keys):
    "user-phone -> list(records whose keys are `keys`)"
    data = {}
    with open(ifile, 'r') as f:
        for l in csv.DictReader(f):
            key = "%s-%s" % (l['name'].replace("Subject", ""), l['phone'])
            if key not in data:
                data[key] = []
            d = np.array([[float(l[i]) for i in keys]]).transpose()
            data[key].append(d)
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
        s = predict(l, touches_A, s_star)
        error = (s - targets_B[i])
        error_sq += np.dot(error.T, error)
    return error_sq / 30.0


def ten_fold_analysis(touches, targets, opts):
    """Operate on user-device, i.e. list of touches and targets"""
    error = []
    for i in range(10):
        error.append(learn_and_err(touches, targets, i * 30, opts))
        print ("%d completed, error: %.6f" % (i, error[-1]))
    return numpy.average(error)


def main(trainingf):
    touches = read_file(trainingf, ['touchX', 'touchY'])['15-Iphone4']
    targets = read_file(trainingf, ['targetX', 'targetY'])['15-Iphone4']

    ten_fold_analysis(touches, targets, d_opts)


if __name__ == '__main__':
    main('courseworkdata.csv')
