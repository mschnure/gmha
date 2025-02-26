source("model/run_systematic.R")
source('calibration/prior_distributions/france_prior.R')
prior = FRANCE.PRIOR
params.start.values = get.default.parameters(location = "France") 
params.start.values = params.start.values[prior@var.names]

params.start.values["trate.0"] = 0.5
params.start.values["trate.1"] = 0.13
params.start.values["trate.2"] = 0.1
params.start.values["trate.3"] = 0.15
params.start.values["trate.4"] = 0.25

params.start.values["age.25.to.49.hiv.mortality.multiplier.0"] = 1
params.start.values["age.25.to.49.hiv.mortality.multiplier.1"] = 1
params.start.values["age.25.to.49.hiv.mortality.multiplier.2"] = 1

save(params.start.values,file=("calibration/starting_values/2025_02_26_france_start_values.Rdata"))

