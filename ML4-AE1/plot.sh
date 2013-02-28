#!/bin/bash -e

if [ $# -eq 0 ]; then
    echo "Need \"a\" or \"b\" as first argument"
    exit 1
fi

echo set terminal pdf
echo set output \"error_$1.pdf\"

echo set xlabel \"parameter $1\"
echo set ylabel \"root mean square error\"
echo "plot \\"
for i in subj_${1}*; do
    echo -n "  "\"$i\" using 1:3 with linespoints title \"
    echo -n $(echo ${i#subj_$1} | sed 's/\.txt//')
    echo \", \\
done

echo 1/0 notitle
