source('model/run_systematic.R')
source("future_projections/create_and_run_interventions_functions.R")
START.TIME = 2022

# applying only to ages 15-24 

# annual.lai.suppression.rate # 0.06497952
# set in model/parameter_mappings/get_lai_art_rates.R

lai.10.from.es.unit = create.intervention.unit(parameter = "LAI.ES.RATES", 
                                               scale = "rate",
                                               start.time = START.TIME,
                                               effect.time = 2030,
                                               effect.value = annual.lai.suppression.rate, # 0.06497952
                                               allow.lower.than.baseline = F)
lai.10.from.eu.unit = create.intervention.unit(parameter = "LAI.EU.RATES", 
                                               scale = "rate",
                                               start.time = START.TIME,
                                               effect.time = 2030,
                                               effect.value = annual.lai.suppression.rate, # 0.06497952
                                               allow.lower.than.baseline = F)
lai.10.from.du.unit = create.intervention.unit(parameter = "LAI.DU.RATES", 
                                               scale = "rate",
                                               start.time = START.TIME,
                                               effect.time = 2030,
                                               effect.value = annual.lai.suppression.rate, # 0.06497952
                                               allow.lower.than.baseline = F)

NO.INTERVENTION = create.intervention.from.units(code="no.int")

lai.10.from.es = create.intervention.from.units(lai.10.from.es.unit,
                                                target.ages = c("15-19","20-24"),
                                                code="lai.es.10")
lai.10.from.eu = create.intervention.from.units(lai.10.from.eu.unit,
                                                target.ages = c("15-19","20-24"),
                                                code="lai.eu.10")
lai.10.from.du = create.intervention.from.units(lai.10.from.du.unit,
                                                target.ages = c("15-19","20-24"),
                                                code="lai.du.10")

# will eventually combine all three arrows
lai.10.from.all = create.intervention.from.units(lai.10.from.es.unit,
                                                 lai.10.from.eu.unit,
                                                 lai.10.from.du.unit,
                                                 target.ages = c("15-19","20-24"),
                                                 code="lai.10.all")

