source('model/run_systematic.R')
source("future_projections/create_and_run_interventions_functions.R")
START.TIME = 2022

load("final_simsets_and_results/all.results_merged_south_africa_2025-08-27.Rdata")
simset.no.int = simset.list.full$no.int

lai.10.from.es.unit = create.intervention.unit(parameter = "LAI.RATES", 
                                               scale = "proportion",
                                               start.time = START.TIME,
                                               effect.time = 2030,
                                               effect.value = .10,
                                               allow.lower.than.baseline = F)

NO.INTERVENTION = create.intervention.from.units(code="no.int")

lai.10.from.es = create.intervention.from.units(lai.10.from.es.unit,
                                                code="lai.es.10")

# will eventually combine all three arrows
lai.10.from.all = create.intervention.from.units(lai.10.from.es,
                                                 #lai.10.from.eu,
                                                 #lai.10.from.du,
                                                 code="lai.10.all")

# simset.lai.es.10 = run.intervention.on.simset(simset.no.int,
#                                               end.year = 2040,
#                                               intervention = lai.10.from.es)
