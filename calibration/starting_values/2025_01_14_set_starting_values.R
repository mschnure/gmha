load("mcmc_runs/simset_kenya_2025-01-16.Rdata")
simset.kenya = simset
params.start.values = simset.kenya@parameters[simset.kenya@n.sim,] 
params.start.values["over.50.aging.factor"] = 1
save(params.start.values,file=("calibration/starting_values/2025_01_17_kenya_start_values.Rdata"))

load("mcmc_runs/simset_south_africa_2025-01-15.Rdata")
simset.sa = simset
params.start.values = simset.sa@parameters[simset.sa@n.sim,] 
params.start.values["over.50.aging.factor"] = 1
save(params.start.values,file=("calibration/starting_values/2025_01_17_south_africa_start_values.Rdata"))

load("mcmc_runs/simset_france_2025-01-15.Rdata")
simset.france = simset
params.start.values = simset.france@parameters[simset.france@n.sim,] 
params.start.values["over.50.aging.factor"] = 1
save(params.start.values,file=("calibration/starting_values/2025_01_17_france_start_values.Rdata"))

# sim.france = run.model.for.parameters(variable.parameters = params.france,location = "France")
# 
# simplot(simset.sa,
#         years = 1980:2030, 
#         show.individual.sims = F)
