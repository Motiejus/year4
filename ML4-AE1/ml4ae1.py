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


def get_miu(c_hat, C_hat, z, sigma_sq=d_sigma_sq):
    N, N = C_hat.shape
    C_hat_with_noise = C_hat + np.identity(N) * sigma_sq
    mul2 = np.linalg.inv(C_hat_with_noise)
    return np.dot(c_hat, np.dot(mul2, z))


def get_x_hat(x, opts=d_opts):
    _, _, _, alpha, _ = opts
    return np.concatenate((
        np.concatenate((x, alpha * x), axis=1),
        np.concatenate((alpha * x, x), axis=1)
        ))


def predict(touches, targets, s_star):
    c = get_c(touches, s_star)
    c_hat = get_x_hat(c)

    C = get_C(touches)
    C_hat = get_x_hat(C)

    z = get_z(targets)

    return get_miu(c_hat, C_hat, z)


def learn(touches, targets, opts):
    """[C_hat + sigma_sq * I] <dot> z
    
    Part of the equation. After multiplying c_hat by resulting matrix,
    you get the prediction"""
    gamma, b, a, alpha, sigma_sq = opts

    C_hat = get_x_hat(get_C(touches), alpha)
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


def main(trainingf, testf):
    touches = read_file(trainingf, ['touchX', 'touchY'])
    targets = read_file(trainingf, ['targetX', 'targetY'])

    test_touches = read_file(testf, ['touchX', 'touchY'])

    for p, pdata in test_touches.items():
        for touch in pdata:
            pred = predict(touches[p], targets[p], touch)
            print("(%.4f:%.4f) -> (%.4f:%.4f); TODO target" %
                    (touch[0], touch[1], pred[0], pred[1]))


if __name__ == '__main__':
    main('training_data.csv', 'testdata.csv')
