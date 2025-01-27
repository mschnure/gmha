params = get.default.parameters(location = "Kenya")

load("calibration/starting_values/2025_01_17_kenya_start_values.Rdata") 
params.start.values.kenya = params.start.values

load("calibration/starting_values/2025_01_17_south_africa_start_values.Rdata") 
params.start.values.sa = params.start.values

load("calibration/starting_values/2025_01_17_france_start_values.Rdata") 
params.start.values.france = params.start.values

cbind(params[names(params.start.values.kenya)],
      params.start.values.kenya,
      params.start.values.sa,
      params.start.values.france)
