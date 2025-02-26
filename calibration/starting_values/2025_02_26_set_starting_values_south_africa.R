load("mcmc_runs/simset_south_africa_2025-02-26.Rdata")
simset.south.africa = simset
params.start.values = simset.south.africa@parameters[simset.south.africa@n.sim,] 

params.start.values["over.80.mortality.intercept.multiplier.male"] = 1.5 #1.004108
params.start.values["over.80.mortality.intercept.multiplier.female"] = 1.4 #0.7921224 (or 1.1) - if I go with 1.4, downweight 80+ mortality even more

params.start.values["age.15.to.24.hiv.mortality.multiplier.1"] = 0.5 # 0.246964410
params.start.values["age.15.to.24.hiv.mortality.multiplier.2"] = 1 # 0.736734

params.start.values["age.25.to.49.hiv.mortality.multiplier.0"] = 1
params.start.values["age.25.to.49.hiv.mortality.multiplier.1"] = 0.6
params.start.values["age.25.to.49.hiv.mortality.multiplier.2"] = 0.01

params.start.values["over.50.hiv.mortality.multiplier.0"] = 25 # 0.555852490
params.start.values["over.50.hiv.mortality.multiplier.1"] = 25 # 4.28453
params.start.values["over.50.hiv.mortality.multiplier.2"] = 25 # 1.139815427

params.start.values["age.25.to.50.aging.factor"] = 1 # 0.793711697


save(params.start.values,file=("calibration/starting_values/2025_02_26_south_africa_start_values.Rdata"))

