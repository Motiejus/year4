set terminal pdf
set output "images/slowdown.pdf"

set xtics 0,4
set ytics 0,0.1
set yrange [0:1]
set grid
set nobox

set xlabel "Number of cores"
set ylabel "Slowdown"

plot "./1-4-64_speedup.txt" using 1:($2/$1) with linespoints title "";
