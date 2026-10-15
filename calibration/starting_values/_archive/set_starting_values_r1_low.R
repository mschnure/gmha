source("model/run_systematic.R")
source('calibration/prior_distributions/unaids_remainder_prior.R')
prior = UNAIDS.REMAINDER.PRIOR
# params.start.values = params.start.values[prior@var.names]

# initial run of 50,000 with 4x prevalence weight
load("mcmc_runs/simset_r1.low_2025-08-09.Rdata")

params.start.values = simset@parameters[simset@n.sim,] 

params.start.values["age.50.and.over.transmission.multiplier.0"] = 0.3 # 0.2059075 
params.start.values["age.50.and.over.transmission.multiplier.1"] = 0.3 # 0.2109254 
params.start.values["age.50.and.over.transmission.multiplier.2"] = 0.2 # 0.4094585 
params.start.values["age.50.and.over.transmission.multiplier.3"] = 0.1 # 0.4155778 

save(params.start.values,file=("calibration/starting_values/2025_08_12_r1_low_start_values.Rdata"))
