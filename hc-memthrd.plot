set xdata time
set timefmt "%Y-%m-%d %H:%M:%S"
show timefmt
set format x "%d/%H:%M"
set title $dat-file
plot $dat-file using 1:3 with lines title 'memory', $dat-file using 1:4 with lines title 'threads'