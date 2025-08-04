source("model/run_systematic.R")
source('calibration/prior_distributions/mozambique_prior.R')
prior = MOZAMBIQUE.PRIOR
# params.start.values = params.start.values[prior@var.names]


# initial run of 100,000 with 1/8 weight
load("mcmc_runs/simset_mozambique_2025-07-29.Rdata")

default.params =  get.default.parameters(location = "Mozambique") 
params.start.values = simset@parameters[simset@n.sim,] 
additional.params = setdiff(names(default.params),names(params.start.values))

save(params.start.values,file=("calibration/starting_values/2025_08_04_mozambique_start_values.Rdata"))
