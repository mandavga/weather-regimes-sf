#!/usr/bin/env Rscript
library("depmixS4") # HMMs fit
library("dplyr")
library("lubridate")

ops <- list()
ops$level = 1000
ops$input <- sprintf('data/hgt-SA-%d-anomaly.rds', ops$level)
ops$outdir <- "new-out/"
ops$input_start <- "1979-01-01"
ops$input_end <- "2022-06-30"
ops$weather_start <- "1979-01-01"
ops$weather_end <- "2022-06-30"
ops$month_start <- "01"
ops$month_end <- "12"
ops$regimes <- 6
ops$pcs <- 10
ops$modeltype <- "annual"
ops$max_iteration <- 20


get_fmod.depmix <- function(model) {
    ## NOTE: this takes a serious amount of time. Each itiration takes time and
    ## the number of iteration to converse is also high Single run with j=1
    ## took 266 iteration to converse, Time taken was in a range of an hour.
    for(j in 1:ops$max_iteration) {
        ##
        set.seed(j*950)

        if (j < ops$max_iteration/2) {
            do.random <- FALSE
        } else {
            do.random <- TRUE
        }

        print(sprintf("Initial run: %d", j))
        ## FIXME: Reduce Time by increasing CPU use?
        fmod.depmix <- fit(mod,emc = em.control(random = do.random),
                           verbose = TRUE) #conrows = conr.nh)  # )#

        if(class(fmod.depmix) == "depmix.fitted") {
            if (!is.na(stringr::str_match(fmod.depmix@message,
                                          "Log likelihood converged"))) {
                print("Solution Converged.")
                return(fmod.depmix)
            }
        }
        print("Solution Not Converged. Retrying...")
    }
    print("Solution Not Converged.")
    return(NULL)
}

## 1#
## // Load processed 500mb-GPH hgt region data and dates
## hgt.synoptic.region <- readRDS(
##     file='weather-gen/hgt_SA.rds')
hgt.synoptic.region <- readRDS(ops$input)

## for a specific WR number
## e.g, 10 PCs
start_date <- as.Date(ops$weather_start)
end_date <- as.Date(ops$weather_end)
first.date.weather <- start_date
last.date.weather <- end_date

dates.weather <- seq(start_date, end_date, by="days")

start_date_synoptic <- as.Date(ops$input_start)
end_date_synoptic <- as.Date(ops$input_end)
dates.synoptic <- seq(start_date_synoptic, end_date_synoptic, by="days")

common.start <- if (start_date > start_date_synoptic) start_date else start_date_synoptic
common.end <- if (end_date < end_date_synoptic) end_date else end_date_synoptic
stopifnot(common.start < common.end)

common.dates <- seq(common.start, common.end, by="days")
common.years.count <- (as.numeric(format(common.end, "%Y")) -
                       as.numeric(format(common.start, "%Y")))
##  FIX: when the rds has different date range then the input here, doesn't work
## overwrites the variable by only considering the common range
identical.dates.idx <- dates.synoptic %in% dates.weather
hgt.synoptic.region <- hgt.synoptic.region[identical.dates.idx,]

if (ops$month_end < ops$month_start) {
    months_end <- ops$month_end + 12
} else {
    months_end <- ops$month_end
}
## change them in the range of 1-12
wet_months <- (seq(ops$month_start, months_end) - 1) %% 12 + 1

wet.dates.ind <- month(common.dates) %in% wet_months
common.dates <- common.dates[wet.dates.ind]

hgt.synoptic.region <- hgt.synoptic.region[wet.dates.ind,]

## 2#
## / Use PCA beforehand
hgt.synoptic.region.pca <- prcomp(hgt.synoptic.region,
                                  center = TRUE, scale = TRUE)
num_eofs <- ops$pcs
synoptic.pcs <- hgt.synoptic.region.pca$x[,1:num_eofs]


## 3#
## / HMMs runs followed by s-NHMMs
## for a specific WR number
num.states <- ops$regimes # WRs number
number.years.long <- 1000 # e.g., 1000 years; 2000 years, etc

## number of chunks of historical periods; e.g., 1 is one set of
## simulation equal to the historical
my.num.sim <- ceiling(number.years.long / common.years.count)
number.years.long2 <- my.num.sim * common.years.count
num.iteration.hmms <- 5 # number of iteration to simulate WRs
lst.WRs.sNHMMs.states <- list()
## HMMs outputs
## e.g, 10 PCs
modHMMs <- depmix(
    lapply(1:num_eofs, function(x) {
        return(as.formula(sprintf("PC%d ~ 1", x)))
    }),
    nstates = num.states,
    family = rep(list(gaussian()), num_eofs),
    ntimes =  nrow(synoptic.pcs),
    data = data.frame(synoptic.pcs))

## this one is also time consuming. Already takes a good chunk of CPU.
fit.modHMMs.depmix <- fit(modHMMs, verbose=TRUE)

rm(modHMMs)
## synoptic.state.assignments <- posterior(fit.modHMMs.depmix)$state
## state sequence (using the Viterbi algorithm)
## weather.state.assignments <- synoptic.state.assignments[dates.synoptic%in%dates.weather]
## state assignments associated with the local weather variables (in case they span a different time period)

## s-NHMMs run

## NOTE: this one takes a resonably long time (few minutes), takes a chunk of
## ram and a chunk of CPU too.
hhmod <- fit.modHMMs.depmix
hhpars <- c(unlist(getpars(hhmod)))
hhconMat <- hhmod@conMat
init.pars <- list()

rm(fit.modHMMs.depmix)
gc()

init.pars[['transition']] <- lapply(hhmod@transition,
                                    function(x) x@parameters$coefficients)
init.pars[['prior']]  <- hhmod@prior@parameters$coefficients #  prob.kmeans.list[[p]]  #
init.pars[['response']] <- lapply(hhmod@response,
                                  function(x) lapply(x, function(y) unlist(y@parameters)))
init.pars[['conMat']] <- hhconMat

## s-NHMM on PCs with seasonality
## function parameters
#'  my.nstates : Number of hidden states of Weather Regimes
#'  my.num.sim : Number of simulation of WRs (1 set of t (number of years), 2 set of t, etc)
#'  my.synoptic.pcs : "Observed" series of PCs for geopotential heights
#'  my.dates.vec : vector of dates
#'  num.iteration.msar : number of simulation of Markov Chain
#'  modeltype : annual, seasonal, interannual
my.nstates <- num.states
my.num.sim <- my.num.sim
my.synoptic.pcs <- synoptic.pcs
my.dates.vec <- common.dates
num.iteration.hmms
modeltype <- ops$modeltype
init.pars <- init.pars

## Prepare simulation output: [number of days (t) * number of sets of simulated WRs (entire stretch, t)] x number of ensemble members
ts.length <- length(my.dates.vec)*my.num.sim
matrix.hmms.seq.states <- array(NA,c(ts.length,num.iteration.hmms))

lst.tpm.probs <- list()
lst.real.eigen.vectors <- list()

## count number of days assigned to the states #
sum.state.days <- matrix(NA,nrow = my.nstates,ncol = num.iteration.hmms)

## Seasonality
if(modeltype == "annual") {
    my.period <- 365
}
if(modeltype == "seasonal") {
    my.period <- 30
}
if(modeltype == "interannual") {
    my.period <- 365
}
## define day of year for all dates
tsteps <- as.numeric(format(my.dates.vec, "%j")) #seq_len(nrow(my.synoptic.pcs))

## ------- transition model -----------------------------
## Seasonal Covariates for hidden states
if(modeltype == "annual") {
    omegaT <- (2*pi*tsteps)/my.period
    my.covar.trans <- data.frame(CosT = cos(omegaT), SinT = sin(omegaT))
    fo <- as.formula(" ~ -1 + CosT + SinT")
    ## fo <- as.formula(" ~ 1 + CosT + SinT")
}else if(modeltype == "seasonal") {
    omegaT <- (2*pi*tsteps)/my.period
    my.covar.trans <- data.frame(CosT = cos(omegaT), SinT = sin(omegaT))
    fo <- as.formula("~ -1 + CosT + SinT")
} else if(modeltype == "interannual") {
    omegaT <- (2*pi*tsteps)/my.period
    my.covar.trans <- data.frame(CosT = cos(omegaT), SinT = sin(omegaT),
                                 t.trend = tsteps)
    fo <- as.formula("~ 1 + CosT + SinT + t.trend")
}

## A list of transition models, each created by a call to transInit. The length
## of this list should be the number of states of the model.
transition <- list()
if(is.null(init.pars)) {
    for(i in 1:my.nstates) {
        transition[[i]] <- transInit(formula = fo,
                                     data = my.covar.trans,
                                     nstates = my.nstates)
    }
} else {
    ## initial transition parameters are provided
    for(i in 1:my.nstates){
        set.seed(i*950)
        par.cosT <- runif(my.nstates,0.01,0.9)
        par.sinT <- 1 - par.cosT
        pstart. <- c(par.cosT,par.sinT) #init.pars$transition[[i]] # intercepts
        pstart.[which(is.infinite(pstart.))] <- 0.9
        pstart.[which(is.nan(pstart.))] <- 0.01
        transition[[i]] <- transInit(formula = fo,
                                     data = my.covar.trans,
                                     nstates = my.nstates,
                                     pstart = pstart.)
    }
}

## ----------------- prior --------------------------
if(is.null(init.pars)) {
    my.prior <- transInit(~1,ns = my.nstates,data=data.frame(1),
                          ps = runif(my.nstates))
}else {
    if(any(init.pars$prior == 0)) {
        my.prior <- transInit(~1,ns = my.nstates,data=data.frame(1),
                              ps = runif(my.nstates))
    }else {
        my.prior <- transInit(~1,ns = my.nstates,data=data.frame(1),
                              ps = init.pars$prior)
    }
}

## #-----State-wise GLM response model ----------------
my.response.models <- list()
if(is.null(init.pars)) {
    for(i in 1:my.nstates) {
        my.response.models[[i]] <- lapply(1:num_eofs, function(x) {
            return(
                GLMresponse(formula = as.formula(sprintf("PC%d ~ 1", x)),
                            data = data.frame(my.synoptic.pcs ),
                            family = gaussian())
                )
        })
    }
} else {
    for(i in 1:my.nstates){
        my.response.models[[i]] <- lapply(1:num_eofs, function(x) {
            return(
                GLMresponse(formula = as.formula(sprintf("PC%d ~ 1", x)),
                            data = data.frame(my.synoptic.pcs ),
                            family = gaussian(),
                            pstart = init.pars$response[[i]][[x]])
                )
        })
    }
}

## ---------------- Initialize ---------------
mod <- makeDepmix(response=my.response.models,
                  transition=transition,
                  prior=my.prior,
                  ntimes= nrow(my.synoptic.pcs),
                  homogeneous=FALSE)

rm(my.response.models)
gc()

## Parameter Estimation : ----------------------------

## NOTE: this one also takes a reasonable amount of time. The CPU use sometimes
## spikes but not much around other times, so it seems to be time intensive
## rather than resource intensive. We can probably do something on this.

fmod.depmix <- get_fmod.depmix(mod)

## Removed mod variable d/t high memory footprint, it doesn't seem to be used
## after this.
rm(mod)
gc()

## -------------------------------
prob.state <- forwardbackward(fmod.depmix)$gamma

## 'Observed' or 'TRUE" state sequence
## state sequnce (using the Viterbi algorithm) #
seq.state <- posterior(fmod.depmix,type='viterbi')$state
## Probability of each state
delta.probs <- posterior(fmod.depmix,type='viterbi') %>% dplyr::select(-state)


lst.WRs.states <- list(fitted.model = fmod.depmix,
                       viterbi.seq = seq.state)

saveRDS(lst.WRs.states,
        file = file.path(ops$outdir, "weather-regimes.rds"))
## save the csv
regimes <- data.frame(date=common.dates, clusters=seq.state)
write.csv(regimes, file.path(ops$outdir, "weather-regimes.csv"))


## Save the clustures center as original data table
library(raster)
library(stringr)

get_raster <- function(df) {
    df <- as.data.frame(df)
    df$lat <- 0
    df$lon <- 0

    for (i in 1:length(df$lonlat)){
        ll <- df$lonlat[i]
        ll_split <- str_split_fixed(ll, ",", n=2)
        lon <- as.numeric(ll_split[1])
        lat <- as.numeric(ll_split[2])
        df$lat[i] <- lat
        df$lon[i] <- lon
    }

    xyz <- subset(df, select = c("lon", "lat", "geopotential_ht"))

    dfr <- rasterFromXYZ(xyz = xyz)
    return(dfr)
}


clusters <- aggregate(hgt.synoptic.region, by=list(seq.state), FUN = mean)

dir.create(file.path(ops$outdir, "rasters"), showWarnings = FALSE)
for (N in 1:num.states){
    df <- data.frame(lonlat = names(clusters), geopotential_ht=as.numeric(clusters[N,]))
    dfr1 <- get_raster(df)
    writeRaster(dfr1, file.path(ops$outdir, sprintf("rasters/cluster-%01d.tif", N)), overwrite=TRUE)
    ## plot(dfr1)
}
