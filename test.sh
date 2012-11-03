#!/bin/sh

set -e

make -s

export CMD=`pwd`/Main

for t in t/p*.alg; do
    $CMD $t | diff -w - ${t%.alg}.ans
    echo "Test ${t%.alg} executed successfully"
done

for t in t/*.sh; do
    $t
    echo "Test ${t%sh} executed successfully"
done
