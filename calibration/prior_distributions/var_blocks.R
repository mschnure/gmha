
PARAMETER.VAR.BLOCKS = list(
    
    # 10/29/26 (master-convergence): transmission parameters grouped BY TIME POINT - each trate with every age group's
    # multiplier for that time point - so the sampler can learn the trade-off between them (trate.k up / all age
    # multipliers .k down gives the same incidence). Previously each trate was alone and the age multipliers were grouped
    # by age (times 1-3 together), which mixed poorly (South Africa: 38 of 77 parameters with R-hat > 1.1).
    transmission.time.0 = c("trate.0",
                             "age.15.to.19.transmission.multiplier.0",
                             "age.20.to.29.transmission.multiplier.0",
                             "age.30.to.39.transmission.multiplier.0",
                             "age.40.to.49.transmission.multiplier.0",
                             "age.50.and.over.transmission.multiplier.0"),
    transmission.time.1 = c("trate.1",
                             "age.15.to.19.transmission.multiplier.1",
                             "age.20.to.29.transmission.multiplier.1",
                             "age.30.to.39.transmission.multiplier.1",
                             "age.40.to.49.transmission.multiplier.1",
                             "age.50.and.over.transmission.multiplier.1"),
    transmission.time.2 = c("trate.2",
                             "age.15.to.19.transmission.multiplier.2",
                             "age.20.to.29.transmission.multiplier.2",
                             "age.30.to.39.transmission.multiplier.2",
                             "age.40.to.49.transmission.multiplier.2",
                             "age.50.and.over.transmission.multiplier.2"),
    transmission.time.3 = c("trate.3",
                             "age.15.to.19.transmission.multiplier.3",
                             "age.20.to.29.transmission.multiplier.3",
                             "age.30.to.39.transmission.multiplier.3",
                             "age.40.to.49.transmission.multiplier.3",
                             "age.50.and.over.transmission.multiplier.3"),
    trate4 = c("trate.4", 
               "proportion.trate.change.by.3.5"),
    
    # who infects whom: sex and age mixing together (10/29/26; previously each alone)
    transmission.mixing = c("female.to.male.multiplier",
                            "age.assortativity"),
    
    birth.transmission = c("birth.transmission.risk.0",
                           "birth.transmission.risk.1"),
    
    testing = c("log.OR.testing.intercept",
                "log.OR.testing.slope"),
    
    engagement.disengagement = c("log.OR.engagement.slope",
                                 "unsuppressed.disengagement.rates",
                                 "suppressed.disengagement.rates"),

    suppression = c("log.OR.suppression.slope",
                    "unsuppression.rates"),
    
    male.cascade.multiplier = c("male.awareness.multiplier",
                                "male.engagement.multiplier",
                                "male.suppression.multiplier"),
    
    general.mortality.male = c("age.45.to.64.mortality.intercept.multiplier.male",
                               "age.45.to.64.mortality.slope.multiplier.male",
                               "age.65.to.79.mortality.intercept.multiplier.male",
                               "age.65.to.79.mortality.slope.multiplier.male",
                               'over.80.mortality.intercept.multiplier.male',
                               'over.80.mortality.slope.multiplier.male'),
    
    general.mortality.female = c("age.45.to.64.mortality.intercept.multiplier.female",
                               "age.45.to.64.mortality.slope.multiplier.female",
                               "age.65.to.79.mortality.intercept.multiplier.female",
                               "age.65.to.79.mortality.slope.multiplier.female",
                               'over.80.mortality.intercept.multiplier.female',
                               'over.80.mortality.slope.multiplier.female'),
    
    hiv.mortality = c("hiv.specific.mortality.rates.0",
                      "hiv.specific.mortality.rates.1",
                      "hiv.specific.mortality.rates.2"),
    
    hiv.mortality.sex = c("male.hiv.mortality.multiplier.0",
                          "male.hiv.mortality.multiplier.1",
                          "male.hiv.mortality.multiplier.2"),
    
    hiv.mortality.age.multipliers.infant = c("age.0.to.4.hiv.mortality.multiplier.0",
                                             "age.5.to.14.hiv.mortality.multiplier.0",
                                             "age.0.to.14.hiv.mortality.multiplier.1",
                                             "age.0.to.14.hiv.mortality.multiplier.2"),
    
    hiv.mortality.age.multipliers.young = c("age.15.to.24.hiv.mortality.multiplier.0",
                                            "age.15.to.24.hiv.mortality.multiplier.1",
                                            "age.15.to.24.hiv.mortality.multiplier.2"),
    
    hiv.mortality.age.multipliers.mid = c("age.25.to.49.hiv.mortality.multiplier.0",
                                            "age.25.to.49.hiv.mortality.multiplier.1",
                                            "age.25.to.49.hiv.mortality.multiplier.2"),
    
    hiv.mortality.age.multipliers.old = c("over.50.hiv.mortality.multiplier.0",
                                          "over.50.hiv.mortality.multiplier.1",
                                          "over.50.hiv.mortality.multiplier.2"),
    
    fertility = c("fertility.multiplier"),
    
    aging.1 = c("age.15.to.19.base.aging.rate",
                "age.20.to.24.base.aging.rate"),
    
    aging.2 = c("age.15.to.19.aging.factor",
                "age.20.to.24.aging.factor",
                "age.25.to.50.aging.factor"#,
                #"over.50.aging.factor"
                )
)
