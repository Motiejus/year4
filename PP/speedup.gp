set terminal pdf
set output "images/speedup.pdf"

set xtics 0,4
set ytics 0,4
set grid
set key right bottom

set xlabel "Number of cores"
set ylabel "Speedup"
plot "./1-4-56_speedup.txt" with linespoints title "Real speedup", \
x title "Maximum theoretical speedup";
