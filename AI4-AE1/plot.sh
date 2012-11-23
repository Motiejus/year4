#!/bin/sh
set -e

E_M=$(cat <<EOF
set terminal latex
set output 'res/aggr_e-m.tex'
set title "Energy vs Magnitude"
set xlabel "Energy"
set ylabel "Magnitude"
plot "samples/summary.txt" using 2:3
EOF
)

E_Z=$(cat <<EOF
set terminal latex
set output 'res/aggr_e-z.tex'
set title "Energy vs Zero Crossing Rate"
set xlabel "Energy"
set ylabel "ZCR"
plot "samples/summary.txt" using 2:4
EOF
)

M_Z=$(cat <<EOF
set terminal latex
set output 'res/aggr_m-z.tex'
set title "Magnitude vs Zero Crossing Rate"
set xlabel "Magnitude"
set ylabel "ZCR"
plot "samples/summary.txt" using 3:4
EOF
)

usage() {
    echo "Usage: `basename $0` e-m | e-z | m-z"
    exit 1
}

if [ $# -lt 1 ]; then
    usage
fi

case $1 in
    e-m) SCRIPT=$E_M
        ;;
    e-z) SCRIPT=$E_Z
        ;;
    m-z) SCRIPT=$M_Z
        ;;
    *) usage
        ;;
esac

make -C samples

echo "$SCRIPT" | gnuplot -persist
