#!/bin/sh
set -e

E_M=$(cat <<EOF
set terminal pdf
set output 'res/aggr_e-m.pdf'
set xlabel "Energy"
set ylabel "Magnitude"
set key left top
plot "< grep ^silence samples/summary.txt" using 2:3 title "Silence", \
    "< grep ^speech samples/summary.txt" using 2:3 title "Speech"
EOF
)

E_Z=$(cat <<EOF
set terminal pdf
set key off
set output 'res/aggr_e-z.pdf'
set xlabel "Energy"
set ylabel "ZCR"
plot "< grep ^silence samples/summary.txt" using 2:4 title "Silence", \
    "< grep ^speech samples/summary.txt" using 2:4 title "Speech"
EOF
)

M_Z=$(cat <<EOF
set terminal pdf
set key off
set output 'res/aggr_m-z.pdf'
set xlabel "Magnitude"
set ylabel "ZCR"
plot "< grep ^silence samples/summary.txt" using 3:4 title "Silence", \
    "< grep ^speech samples/summary.txt" using 3:4 title "Speech"
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
