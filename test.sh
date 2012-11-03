#!/bin/sh

set -e

make -s

for t in t/*.alg; do
    ./Main $t | diff -w - ${t%.alg}.ans
    echo "Test ${t%.alg} executed successfully"
done
