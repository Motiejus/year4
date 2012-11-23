#!/bin/sh

set -e

make -s

export CMD=`pwd`/Main

for t in t/*.alg; do
    BASE=${t%.alg}
    if [ -f "${BASE}.in" ]; then
        $CMD $t < "${BASE}.in" | diff -w - "${BASE}.ans"
    else
        $CMD $t | diff -w - "${BASE}.ans"
    fi
    echo "Test ${BASE} executed successfully"
done
