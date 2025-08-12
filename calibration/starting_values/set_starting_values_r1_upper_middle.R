source("model/run_systematic.R")
source('calibration/prior_distributions/unaids_remainder_prior.R')
prior = UNAIDS.REMAINDER.PRIOR
# params.start.values = params.start.values[prior@var.names]

# initial run of 50,000 with 4x prevalence weight
load("mcmc_runs/simset_r1.upper.middle_2025-08-09.Rdata")

params.start.values = simset@parameters[simset@n.sim,] 

params.start.values["age.50.and.over.transmission.multiplier.1"] = 0.25 # 0.07704944  
params.start.values["age.50.and.over.transmission.multiplier.2"] = 0.1 # 0.07704944   

save(params.start.values,file=("calibration/starting_values/2025_08_12_r1_upper_middle_start_values.Rdata"))
