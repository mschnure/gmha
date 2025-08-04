source("model/run_systematic.R")
source('calibration/prior_distributions/kenya_prior.R')
prior = KENYA.PRIOR
# params.start.values = params.start.values[prior@var.names]


# initial run of 100,000 with 1/8 weight
load("mcmc_runs/simset_kenya_2025-08-03.Rdata")

default.params =  get.default.parameters(location = "Kenya") 
params.start.values = simset@parameters[simset@n.sim,] 
additional.params = setdiff(names(default.params),names(params.start.values))

save(params.start.values,file=("calibration/starting_values/2025_08_04_kenya_start_values.Rdata"))
