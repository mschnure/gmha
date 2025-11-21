source('model/run_systematic.R')
source("future_projections/create_and_run_interventions_functions.R")

PROB.5.YEAR = 0.25 # this is the % uptake you want over 5 years 
# applying only to ages 15-24

source("model/parameter_mappings/get_lai_art_rates_intervention.R")
# print(rbind(annual.lai.suppression.rate.es, # 1 week to get on LAI 
#             annual.lai.suppression.rate.eu, # 3 months to suppress 
#             annual.lai.suppression.rate.du)) # 4 months to start ART, 3 months to suppress 
# calculated in model/parameter_mappings/get_lai_art_rates_intervention.R, based on PROB.5.YEAR above 

START.TIME = 2022
lai.from.es.unit = create.intervention.unit(parameter = "LAI.ES.RATES", 
                                            scale = "rate",
                                            start.time = START.TIME, # time when intervention starts scaling up
                                            effect.time = 2023, # times when intervention reaches value
                                            end.time = 2027, # when intervention should end, default to Inf
                                                # change to 2026, unless I make the below removal instantaneous 
                                            effect.value = annual.lai.suppression.rate.es, 
                                            allow.lower.than.baseline = F)
lai.from.eu.unit = create.intervention.unit(parameter = "LAI.EU.RATES", 
                                            scale = "rate",
                                            start.time = START.TIME,
                                            effect.time = 2023,
                                            end.time = 2027,
                                            effect.value = annual.lai.suppression.rate.eu, 
                                            allow.lower.than.baseline = F)
lai.from.du.unit = create.intervention.unit(parameter = "LAI.DU.RATES", 
                                            scale = "rate",
                                            start.time = START.TIME,
                                            effect.time = 2023,
                                            end.time = 2027, 
                                            effect.value = annual.lai.suppression.rate.du, 
                                            allow.lower.than.baseline = F)

lai.disengagement.unit = create.intervention.unit(parameter = "LAI.DISENGAGEMENT.RATES", # removal 
                                                  scale = "rate",
                                                  start.time = 2027, # start in 2027
                                                  effect.time = 2028, # reach full effect in 1 year; maybe change to 2027
                                                  effect.value = 52, # basically instantaneous rate removing them? 
                                                  allow.lower.than.baseline = F)


NO.INTERVENTION = create.intervention.from.units(code="no.int")

lai.from.es = create.intervention.from.units(lai.from.es.unit,
                                             target.ages = c("15-19","20-24"),
                                             code="lai.es")
lai.from.eu = create.intervention.from.units(lai.from.eu.unit,
                                             target.ages = c("15-19","20-24"),
                                             code="lai.eu")
lai.from.du = create.intervention.from.units(lai.from.du.unit,
                                             target.ages = c("15-19","20-24"),
                                             code="lai.du")

# combine all three arrows
lai.from.all = create.intervention.from.units(lai.from.es.unit,
                                              lai.from.eu.unit,
                                              lai.from.du.unit,
                                              target.ages = c("15-19","20-24"),
                                              code="lai.all")

lai.disengagement = create.intervention.from.units(lai.disengagement.unit,
                                                   code="lai.all")

# make the analogous removal intervention that removes 25+ starting 2022

lai.from.all.with.disengagement.old = create.intervention.from.units(lai.from.es.unit,
                                                                 lai.from.eu.unit,
                                                                 lai.from.du.unit,
                                                                 lai.disengagement.unit, # I want this to apply to all ages
                                                                 target.ages = c("15-19","20-24"),
                                                                 code="lai.all")




