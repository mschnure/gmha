load("mcmc_runs/simset_kenya_2025-02-26.Rdata")
simset.kenya = simset
params.start.values = simset.kenya@parameters[simset.kenya@n.sim,] 

save(params.start.values,file=("calibration/starting_values/2025_02_26_kenya_start_values.Rdata"))

