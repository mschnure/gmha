
PARAMETER.VAR.BLOCKS = list(
    
    trate0 = "trate.0",
    trate1 = "trate.1",
    trate2 = "trate.2",
    trate3 = "trate.3", 
    trate4 = c("trate.4", 
               "proportion.trate.change.by.3.5"),
    
    sex.transmission.multiplier = c("female.to.male.multiplier"),
    
    age.transmission.multipliers.00 = c("age.15.to.19.transmission.multiplier.0"),
    
    age.transmission.multipliers.01 = c("age.20.to.29.transmission.multiplier.0"),
    
    age.transmission.multipliers.02 = c("age.40.to.49.transmission.multiplier.0"),
    
    
    age.transmission.multipliers.1 = c("age.15.to.19.transmission.multiplier.1",
                                       "age.15.to.19.transmission.multiplier.2",
                                       "age.15.to.19.transmission.multiplier.3"),
    
    age.transmission.multipliers.2 = c("age.20.to.29.transmission.multiplier.1",
                                       "age.20.to.29.transmission.multiplier.2",
                                       "age.20.to.29.transmission.multiplier.3"),
    
    age.transmission.multipliers.3 = c("age.40.to.49.transmission.multiplier.1",
                                       "age.40.to.49.transmission.multiplier.2",
                                       "age.40.to.49.transmission.multiplier.3"),
    
    age.transmission.multipliers.4 = c("age.50.and.over.transmission.multiplier.0",
                                       "age.50.and.over.transmission.multiplier.1",
                                       "age.50.and.over.transmission.multiplier.2",
                                       "age.50.and.over.transmission.multiplier.3"),
    
    
    age.assortativity = c("age.assortativity"),
    
    birth.transmission = c("birth.transmission.risk.0",
                           "birth.transmission.risk.1"),
    
    testing = c("log.OR.testing.intercept",
                "log.OR.testing.slope"),
    
    engagement = c("log.OR.engagement.intercept",
                   "log.OR.engagement.pre.universal.slope",
                   "log.OR.engagement.intermediate.slope",
                   "log.OR.engagement.post.universal.slope"),
    
    
    disengagement = c("unsuppressed.disengagement.rates",
                      "suppressed.disengagement.rates"),
    
    suppression = c("suppression.rate.0",
                    "suppression.rate.1",
                    "unsuppression.rates"),
    
    male.cascade.multiplier = c("male.awareness.multiplier",
                                "male.engagement.multiplier",
                                "male.suppression.multiplier"),
    
    general.mortality = c("age.45.to.65.mortality.intercept.multiplier",
                          "age.45.to.65.mortality.slope.multiplier",
                          "over.65.mortality.intercept.multiplier",
                          "over.65.mortality.slope.multiplier"),
    
    hiv.mortality = c("hiv.specific.mortality.rates.0",
                      "hiv.specific.mortality.rates.1",
                      "hiv.specific.mortality.rates.2"),
    
    hiv.mortality.sex = c("male.hiv.mortality.multiplier.0",
                          "male.hiv.mortality.multiplier.1",
                          "male.hiv.mortality.multiplier.2"),
    
    hiv.mortality.age.multipliers.infant = c("age.0.to.14.hiv.mortality.multiplier.0",
                                             "age.0.to.14.hiv.mortality.multiplier.1",
                                             "age.0.to.14.hiv.mortality.multiplier.2"),
    hiv.mortality.age.multipliers.young = c("age.15.to.24.hiv.mortality.multiplier.0",
                                            "age.15.to.24.hiv.mortality.multiplier.1",
                                            "age.15.to.24.hiv.mortality.multiplier.2"),
    
    hiv.mortality.age.multipliers.old = c("over.50.hiv.mortality.multiplier.0",
                                          "over.50.hiv.mortality.multiplier.1",
                                          "over.50.hiv.mortality.multiplier.2"),
    
    fertility = c("fertility.multiplier"),
    
    aging.1 = c("age.15.to.19.base.aging.rate",
                "age.20.to.24.base.aging.rate"),
    
    aging.2 = c("age.15.to.19.aging.factor",
                "age.20.to.24.aging.factor",
                "age.25.to.50.aging.factor",
                "over.50.aging.factor")
)
