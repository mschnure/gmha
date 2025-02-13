load("mcmc_runs/simset_south_africa_2025-02-13.Rdata")
simset.south.africa = simset
params.start.values = simset.south.africa@parameters[simset.south.africa@n.sim,] 


params.manual["birth.transmission.risk.0"] = 0.6 # arrived at 0.4462366, started at 0.42 
params.mcmc["birth.transmission.risk.1"] = 0.1 # arrived at 0.06844263, started at 0.3

params.manual["age.0.to.4.hiv.mortality.multiplier.0"] = 1 # 0.369758
params.manual["age.5.to.14.hiv.mortality.multiplier.0"] = 1 # 0.2776069
params.manual["age.0.to.14.hiv.mortality.multiplier.1"] = 2 # 1.131516
params.manual["age.0.to.14.hiv.mortality.multiplier.2"] = 2 # 1.478342

params.manual["age.15.to.19.base.aging.rate"] = 0.25 # 0.2318883 # originally 0.25
params.manual["age.20.to.24.base.aging.rate"] = 0.25# 0.2174095 # originally 0.25
params.manual["age.15.to.19.aging.factor"] = 2 # 0.8722751 # originally 2
params.manual["age.20.to.24.aging.factor"] = 2 # 1.101758 # originally 2
params.manual["age.25.to.50.aging.factor"] = 1 # 1.004355 # originally 2
params.manual["over.50.aging.factor"] # 0.9356061 # originally 1

# 
# params.manual["age.15.to.24.hiv.mortality.multiplier.0"] = 2 # 0.458771
# params.manual["age.15.to.24.hiv.mortality.multiplier.1"] = 1 # 0.264008
# params.manual["age.15.to.24.hiv.mortality.multiplier.2"] = 1 # 1.099842 
# 
# params.manual["age.20.to.29.transmission.multiplier.0"] # 1.065732
# params.manual["age.20.to.29.transmission.multiplier.1"] # 1.454948
# params.manual["age.20.to.29.transmission.multiplier.2"] # 1.445206
# params.manual["age.20.to.29.transmission.multiplier.3"] # 1.539011

# params.manual["age.15.to.19.transmission.multiplier.0"] = 1.3 # 1.349588
params.manual["age.15.to.19.transmission.multiplier.1"] = 1.25 # 1.177967
# params.manual["age.15.to.19.transmission.multiplier.2"] # 1.171659
# params.manual["age.15.to.19.transmission.multiplier.3"] # 5.013794

# 
# params.manual["age.40.to.49.transmission.multiplier.0"] = .9 # 0.7318828
# params.manual["age.40.to.49.transmission.multiplier.1"]  # 2.39416
# params.manual["age.40.to.49.transmission.multiplier.2"]  # 2.492292
# params.manual["age.40.to.49.transmission.multiplier.3"] # 2.815547

save(params.start.values,file=("calibration/starting_values/2025_02_13_south_africa_start_values.Rdata"))

