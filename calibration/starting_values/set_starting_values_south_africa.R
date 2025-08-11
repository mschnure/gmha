source("model/run_systematic.R")
source('calibration/prior_distributions/south_africa_prior.R')
prior = SOUTH.AFRICA.PRIOR
# params.start.values = params.start.values[prior@var.names]


# initial run of 100,000 with 1/8 weight
  #load("mcmc_runs/simset_south_africa_2025-07-30.Rdata")
# after that ^ initial run, ran 50,000 with 4x prevalence weight
load("mcmc_runs/simset_south_africa_2025-08-10.Rdata")

default.params =  get.default.parameters(location = "South Africa") 
params.start.values = simset@parameters[simset@n.sim,] 
additional.params = setdiff(names(default.params),names(params.start.values))

#save(params.start.values,file=("calibration/starting_values/2025_08_04_south_africa_start_values.Rdata"))
save(params.start.values,file=("calibration/starting_values/2025_08_11_south_africa_start_values.Rdata"))