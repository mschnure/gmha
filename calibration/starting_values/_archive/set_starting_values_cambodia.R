source("model/run_systematic.R")
source('calibration/prior_distributions/cambodia_prior.R')
prior = CAMBODIA.PRIOR
# params.start.values = params.start.values[prior@var.names]


# initial run of 100,000 with 1/8 weight; still has Kenya age mixing 
load("mcmc_runs/simset_cambodia_2025-07-30.Rdata")

default.params =  get.default.parameters(location = "Cambodia") 
params.start.values = simset@parameters[simset@n.sim,] 
additional.params = setdiff(names(default.params),names(params.start.values))

save(params.start.values,file=("calibration/starting_values/2025_08_04_cambodia_start_values.Rdata"))
