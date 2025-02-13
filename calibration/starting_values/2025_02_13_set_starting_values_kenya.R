load("mcmc_runs/simset_kenya_2025-02-13.Rdata")
simset.kenya = simset
params.start.values = simset.kenya@parameters[simset.kenya@n.sim,] 

params.start.values["birth.transmission.risk.0"] = 0.6 # arrived at 0.3165901, started at 0.42 
params.start.values["birth.transmission.risk.1"] # arrived at 0.4019852, started at 0.3

params.start.values["age.0.to.4.hiv.mortality.multiplier.0"] = 3 # 1.107794
params.start.values["age.5.to.14.hiv.mortality.multiplier.0"] = 3 # 1.553792
params.start.values["age.0.to.14.hiv.mortality.multiplier.1"] = 3 # 1.227954
params.start.values["age.0.to.14.hiv.mortality.multiplier.2"] # 5.485132

params.start.values["age.15.to.19.transmission.multiplier.0"] = 0.9 # 1.029681
params.start.values["age.15.to.19.transmission.multiplier.1"] = 0.4 # 0.352174
params.start.values["age.15.to.19.transmission.multiplier.2"] = 0.3 # 0.2542788
params.start.values["age.15.to.19.transmission.multiplier.3"] # 1.756702

params.start.values["age.40.to.49.transmission.multiplier.0"] = 0.95 # 0.8843208
params.start.values["age.40.to.49.transmission.multiplier.1"] = 4.5 # 0.3738723
params.start.values["age.40.to.49.transmission.multiplier.2"] = 1 # 0.9198105
params.start.values["age.40.to.49.transmission.multiplier.3"] # 2.564686

save(params.start.values,file=("calibration/starting_values/2025_02_13_kenya_start_values.Rdata"))

