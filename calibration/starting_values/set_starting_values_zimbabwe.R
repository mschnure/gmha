source("model/run_systematic.R")
source('calibration/prior_distributions/zimbabwe_prior.R')
prior = ZIMBABWE.PRIOR
# params.start.values = params.start.values[prior@var.names]


# initial run of 100,000 with 1/8 weight
load("mcmc_runs/simset_zimbabwe_2025-08-02.Rdata")

default.params =  get.default.parameters(location = "Zimbabwe") 
params.start.values = simset@parameters[simset@n.sim,] 
additional.params = setdiff(names(default.params),names(params.start.values))

save(params.start.values,file=("calibration/starting_values/2025_08_04_zimbabwe_start_values.Rdata"))
