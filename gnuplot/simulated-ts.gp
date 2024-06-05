set datafile separator ","

set terminal png noenhance size 1000,700
set output "ts-simulated_eerste.png"

set xdata time
set timefmt "%Y"

calc_date(row)=(strptime("%Y-%m-%d", "1979-01-01")+row*24*60*60)

plot for [i=1:50] "../new-out/simulations/sim_".i."/annual_mean.csv"\
     u 1:"precip" w lines lw .1 notitle,\
"../new-out/historical_annual_mean.csv" u "Date":"precip" w lines lw 4 lc "red" title "Eerste"
