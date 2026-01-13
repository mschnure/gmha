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
NO.INTERVENTION = create.intervention.from.units(code="no.int")

# Individual arrows, base rates 
lai.from.es.unit = create.intervention.unit(parameter = "LAI.ES.RATES", 
                                            scale = "rate",
                                            start.time = START.TIME, # time when intervention starts scaling up
                                            effect.time = 2023, # times when intervention reaches value
                                            end.time = 2027, # when intervention should end, default to Inf
                                            effect.value = final.rate.baseline['ES'], 
                                            allow.lower.than.baseline = F)
lai.from.eu.unit = create.intervention.unit(parameter = "LAI.EU.RATES", 
                                            scale = "rate",
                                            start.time = START.TIME,
                                            effect.time = 2023,
                                            end.time = 2027,
                                            effect.value = final.rate.baseline['EU'], 
                                            allow.lower.than.baseline = F)
lai.from.du.unit = create.intervention.unit(parameter = "LAI.DU.RATES", 
                                            scale = "rate",
                                            start.time = START.TIME,
                                            effect.time = 2023,
                                            end.time = 2027, 
                                            effect.value = final.rate.baseline['DU'], 
                                            allow.lower.than.baseline = F)
# EU and DU, using ES rate (faster)
lai.from.es.direct.unit = create.intervention.unit(parameter = "LAI.ES.RATES", 
                                                  scale = "rate",
                                                  start.time = START.TIME,
                                                  effect.time = 2023,
                                                  end.time = 2027,
                                                  effect.value = final.rate.direct['ES'], # changed this 
                                                  allow.lower.than.baseline = F)
lai.from.eu.direct.unit = create.intervention.unit(parameter = "LAI.EU.RATES", 
                                            scale = "rate",
                                            start.time = START.TIME,
                                            effect.time = 2023,
                                            end.time = 2027,
                                            effect.value = final.rate.direct['EU'], # changed this 
                                            allow.lower.than.baseline = F)
lai.from.du.direct.unit = create.intervention.unit(parameter = "LAI.DU.RATES", 
                                            scale = "rate",
                                            start.time = START.TIME,
                                            effect.time = 2023,
                                            end.time = 2027, 
                                            effect.value = final.rate.direct['DU'], # changed this 
                                            allow.lower.than.baseline = F)

# Removal 
lai.removal.by.age.unit = create.intervention.unit(parameter = "LAI.REMOVAL.RATES", 
                                                   scale = "rate",
                                                   start.time = START.TIME, # start in 2022 - immediately start removing 25+ 
                                                   effect.time = START.TIME, 
                                                   effect.value = 52, 
                                                   allow.lower.than.baseline = F)

## Add age component and combine units 
lai.from.es = create.intervention.from.units(lai.from.es.unit,
                                             target.ages = c("15-19","20-24"),
                                             code="lai.es")
lai.from.eu = create.intervention.from.units(lai.from.eu.unit,
                                             target.ages = c("15-19","20-24"),
                                             code="lai.eu")
lai.from.du = create.intervention.from.units(lai.from.du.unit,
                                             target.ages = c("15-19","20-24"),
                                             code="lai.du")
# Direct versions of EU and DU
lai.from.es.direct = create.intervention.from.units(lai.from.es.direct.unit,
                                                   target.ages = c("15-19","20-24"),
                                                   code="lai.es.direct")
lai.from.eu.direct = create.intervention.from.units(lai.from.eu.direct.unit,
                                                   target.ages = c("15-19","20-24"),
                                                   code="lai.eu.direct")
lai.from.du.direct = create.intervention.from.units(lai.from.du.direct.unit,
                                                   target.ages = c("15-19","20-24"),
                                                   code="lai.du.direct")


# combine all three arrows
lai.from.all = create.intervention.from.units(lai.from.es.unit,
                                              lai.from.eu.unit,
                                              lai.from.du.unit,
                                              target.ages = c("15-19","20-24"),
                                              code="lai.all")

lai.from.all.direct = create.intervention.from.units(lai.from.es.direct.unit,
                                                    lai.from.eu.direct.unit,
                                                    lai.from.du.direct.unit,
                                                    target.ages = c("15-19","20-24"),
                                                    code="lai.all.direct")

lai.removal.by.age = create.intervention.from.units(lai.removal.by.age.unit, # this unit starts in 2022
                                                    target.ages = MODEL.TO.SURVEILLANCE.AGE.MAPPING$`25 and over`,
                                                    code="lai.rem.by.age")


# lai.removal.after.5.years.unit = create.intervention.unit(parameter = "LAI.REMOVAL.RATES", 
#                                                           scale = "rate",
#                                                           start.time = 2027, # start in 2027
#                                                           effect.time = 2027, # instantaneous
#                                                           effect.value = 52, # one week to remove them 
#                                                           allow.lower.than.baseline = F)

# lai.removal.after.5.years = create.intervention.from.units(lai.removal.after.5.years.unit, # this unit starts in 2027 
#                                                            code="lai.rem.by.time")









