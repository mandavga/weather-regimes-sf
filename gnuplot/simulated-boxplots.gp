set datafile separator ","

set style data boxplot
set xtics 1

set terminal png noenhance size 1000,700
set output "simulation_stats.png"

set key default
set key box
set key bottom right
set multiplot layout 1,3 title "Precipitation stats for SabieSands (simulated and historical)"
# set yrange [0:]
# set terminal png noenhance size 1000,700
# set output "changed-simulations-stats.png"

do for [col=1:3] {
    colname=word("mean sd skew", col)
    set title colname
plot "../new-out/simulations-stats.csv"\
     u (col):colname title \
     "Simulated",\
     "../new-out/historical-stats.csv"\
     u (col):colname with points ls 7 ps 1.2 title\
     "Historical",\
     "../new-out/historical-stats.csv"\
     u (col):colname:(sprintf("%.2f", column(colname))) with labels\
     offset 5,0 notitle,\
     "../new-out/simulation-mean.csv"\
     u (col):colname with points ls 7 lc "orange" title\
     "Sim Mean",\
     "../new-out/simulation-mean.csv"\
     u (col):colname:(sprintf("%.2f", column(colname))) with labels\
     offset -5,0 notitle,\
}

unset multiplot