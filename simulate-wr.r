historic.start <- as.Date("1979-01-01")
historic.end <- as.Date("2022-06-30")
historic.dates <- seq(historic.start, historic.end, by="days")
historic.years.count <- (as.numeric(format(historic.end, "%Y")) -
                       as.numeric(format(historic.start, "%Y")))

number.years.long <- 1000 # e.g., 1000 years; 2000 years, etc
## number of chunks of historical periods; e.g., 1 is one set of
## simulation equal to the historical
my.num.sim <- ceiling(number.years.long / historic.years.count)
number.years.long2 <- my.num.sim*historic.years.count
num.iteration.hmms <- 10

ts.length <- length(historic.dates)*my.num.sim
matrix.hmms.seq.states <- array(NA, c(ts.length, num.iteration.hmms))

model.variables <- readRDS("new-out/weather-regimes.rds")
fmod.depmix <- model.variables$fitted.model

for (it.cnt in 1:num.iteration.hmms) {
    sim.fmod <- depmixS4::simulate(fmod.depmix, nsim = my.num.sim, seed = it.cnt)
    
    sim.seq.state <- sim.fmod@states # different from viterbi sequence 
    
    matrix.hmms.seq.states[,it.cnt] <- sim.seq.state
    
    prct.done <- round(it.cnt/num.iteration.hmms*100, digits = 2)
    print(sprintf("---finishing %d out of %d (%.2f%%)",
                  it.cnt, num.iteration.hmms,
                  prct.done))
}

simulated <- as.data.frame(matrix.hmms.seq.states)
names(simulated) <- mapply(function(x) {sprintf("sim_%d", x)} , 1:num.iteration.hmms)
simulated$ref_date <- rep(historic.dates, my.num.sim)
head(simulated)
tail(simulated)

write.csv(simulated, "new-out/simulated-wr.csv", row.names=FALSE)
