source("model/run_systematic.R")
source('calibration/prior_distributions/france_prior.R')
prior = FRANCE.PRIOR
# params.start.values = params.start.values[prior@var.names]


# This was a limited run:
# Incidence and prevalence targets only included totals 
# Therefore, didn't include transmission multipliers by age 
# Also, unsure about testing model so used Kenya instead, didn't include awareness target, and didn't include testing multipliers

default.params =  get.default.parameters(location = "France") 
params.start.values = simset@parameters[simset@n.sim,] 
additional.params = setdiff(names(default.params),names(params.start.values))

transmission.multipliers = additional.params[grepl("transmission.multiplier",additional.params)]
testing.multipliers = additional.params[grepl("testing",additional.params)]

params.start.values[transmission.multipliers] = 1
params.start.values[testing.multipliers] = 0

save(params.start.values,file=("calibration/starting_values/2025_06_19_france_start_values.Rdata"))
