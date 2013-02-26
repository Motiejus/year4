#!/usr/bin/env python

import csv

import numpy as np

d_y = 0.05
d_b = 5
d_a = 0.1
d_alpha = 0.9
d_sigma_sq = 1e-3

def distance(sn, sm):
    return sum(np.square(np.subtract(sn, sm)))


def Cov(sn, sm, a=d_a, b=d_b, y=d_y):
    "Covariance function"
    return b * np.add(
            a * np.dot(np.transpose(sn), sm),
            (1 - a) * np.exp(-y * distance(sn, sm))
            )


def get_z(targets):
    "[target, ...] -> 1D locations vector"
    N = len(targets)
    ret = np.zeros(N * 2)
    for i, (x, y) in enumerate(targets):
        ret[i] = x
        ret[i + N] = y
    return np.transpose([ret])


def get_C(touches):
    "covariance for initial (touch) data"
    N = len(touches)
    ret = np.zeros(N * N).reshape(N, N)
    for i, sn in enumerate(touches):
        for j, sm in enumerate(touches):
            ret[i, j] = Cov(sn, sm)
    return ret


def get_c(touches, s_star):
    "covariance between s* and N (touch) input vectors in training set"
    N = len(touches)
    ret = np.zeros(N)
    for i, si in enumerate(touches):
        ret[i] = Cov(s_star, si)
    return np.array([ret])


def get_miu(c_hat, C_hat, z, sigma_sq=d_sigma_sq):
    N, N = C_hat.shape
    C_hat_with_noise = C_hat + np.identity(N) * sigma_sq
    mul2 = np.linalg.inv(C_hat_with_noise)
    return np.dot(np.dot(c_hat, mul2), z)


def get_x_hat(x, alpha=d_alpha):
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


def read_file(ifile, keys):
    "user-phone -> list(records whose keys are `keys`)"
    data = {}
    with open(ifile, 'r') as f:
        for l in csv.DictReader(f):
            subj = l['name'].replace("Subject", "")
            key = "%s-%s" % (subj, l['phone'])
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
