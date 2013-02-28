#!/bin/bash -e

if [ $# -eq 0 ]; then
    echo "Need \"a\" or \"b\" as first argument"
    exit 1
fi

echo "plot \\"
for i in subj_${1}*; do
    echo "  "\"$i\" using 1:3 with linespoints title \"${i#subj_}\", \\
done

echo 1/0 notitle
