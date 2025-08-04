source("model/run_systematic.R")
source('calibration/prior_distributions/uganda_prior.R')
prior = UGANDA.PRIOR
# params.start.values = params.start.values[prior@var.names]


# initial run of 100,000 with 1/8 weight
load("mcmc_runs/simset_tanzania_2025-07-31.Rdata")

default.params =  get.default.parameters(location = "Uganda") 
params.start.values = simset@parameters[simset@n.sim,] 
additional.params = setdiff(names(default.params),names(params.start.values))

save(params.start.values,file=("calibration/starting_values/2025_08_04_uganda_start_values.Rdata"))
