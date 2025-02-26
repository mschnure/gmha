load("mcmc_runs/simset_kenya_2025-02-26.Rdata")
simset.kenya = simset
params.start.values = simset.kenya@parameters[simset.kenya@n.sim,] 

params.start.values["age.25.to.49.hiv.mortality.multiplier.0"] = 1
params.start.values["age.25.to.49.hiv.mortality.multiplier.1"] = 1
params.start.values["age.25.to.49.hiv.mortality.multiplier.2"] = 1

save(params.start.values,file=("calibration/starting_values/2025_02_26_kenya_start_values.Rdata"))

