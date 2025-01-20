source('calibration/make_joint_distribution.R')

FRANCE.PRIOR = join.distributions(
    
    # general 
    trates = make.joint.distribution(median.r2 = 0.10, # 2008 ## CHANGED 1/10 (from 0.25)
                                     sd.r2=log(4)/2,
                                     median.r0.to.r1 = 4, # 1990 relative to 1997 ## CHANGED 1/10 (from 8)
                                     sd.r0.to.r1 = log(4)/2, ## CHANGED 1/10 (from log(2)/2)
                                     median.r1.to.r2 = 1, # 1997 relative to 2008
                                     sd.r1.to.r2 = log(4)/2, ## CHANGED 1/10 (from log(2)/2)
                                     median.r3.to.r2 = 1, # 2018 relative to 2008
                                     sd.r3.to.r2 = log(4)/2, ## CHANGED 1/10 (from log(2)/2)
                                     median.r4.to.r3 = 1, # 2040 relative to 2018
                                     sd.r4.to.r3 = log(4)/2), ## CHANGED 1/10 (from log(2)/2)
    
    # trate.0 = Lognormal.Distribution(log(.5), log(8)/2),  
    # trate.1 = Lognormal.Distribution(log(.25), log(8)/2), 
    # trate.2 = Lognormal.Distribution(log(.25), log(8)/2), 
    # trate.3 = Lognormal.Distribution(log(.25), log(8)/2), 
    
    proportion.trate.change.by.3.5 = Beta.Distribution(alpha = 21,beta = 7),
    
    # sex transmission multipliers
    female.to.male.multiplier = Lognormal.Distribution(log(1), log(4)/2), 
    
    # age transmission multipliers
    age.15.to.19.transmission.multiplier.0 = Lognormal.Distribution(log(1), log(4)/2),
    age.15.to.19.transmission.multiplier.1 = Lognormal.Distribution(log(1), log(4)/2),
    age.15.to.19.transmission.multiplier.2 = Lognormal.Distribution(log(1), log(4)/2),
    age.15.to.19.transmission.multiplier.3 = Lognormal.Distribution(log(1), log(4)/2),
    
    age.20.to.29.transmission.multiplier.0 = Lognormal.Distribution(log(1), log(4)/2),
    age.20.to.29.transmission.multiplier.1 = Lognormal.Distribution(log(1), log(4)/2),
    age.20.to.29.transmission.multiplier.2 = Lognormal.Distribution(log(1), log(4)/2),
    age.20.to.29.transmission.multiplier.3 = Lognormal.Distribution(log(1), log(4)/2),
    
    age.40.to.49.transmission.multiplier.0 = Lognormal.Distribution(log(1), log(4)/2),
    age.40.to.49.transmission.multiplier.1 = Lognormal.Distribution(log(1), log(4)/2),
    age.40.to.49.transmission.multiplier.2 = Lognormal.Distribution(log(1), log(4)/2),
    age.40.to.49.transmission.multiplier.3 = Lognormal.Distribution(log(1), log(4)/2),
    
    age.50.and.over.transmission.multiplier.0 = Lognormal.Distribution(log(1), log(4)/2),
    age.50.and.over.transmission.multiplier.1 = Lognormal.Distribution(log(1), log(4)/2),
    age.50.and.over.transmission.multiplier.2 = Lognormal.Distribution(log(1), log(4)/2),
    age.50.and.over.transmission.multiplier.3 = Lognormal.Distribution(log(1), log(4)/2), 
    
    # other transmission multipliers
    age.assortativity = Lognormal.Distribution(log(1), log(1.5)/2), 
    
    birth.transmission.risk.0 = Logitnormal.Distribution(logit(0.42), log(3)/2), 
    birth.transmission.risk.1 = Logitnormal.Distribution(logit(0.3), log(3)/2), 
    # because logit, this means off by an *OR* of 4 (as opposed to just a multiplier of 4)
    # can arbitrarily pick SD to include what's included in the paper 
    # because birth transmission risk is a proportion, either logit normal or beta 
    # (for any beta, there is a logit normal that approximates)
    
    # cascade parameters
    log.OR.testing.intercept = Normal.Distribution(0, log(4)/2), 
    log.OR.testing.slope = Normal.Distribution(0, log(4)/2),
    
    log.OR.engagement.intercept = Normal.Distribution(0, log(4)/2),
    log.OR.engagement.pre.universal.slope = Normal.Distribution(0, log(4)/2),
    log.OR.engagement.intermediate.slope = Normal.Distribution(0, log(4)/2),
    log.OR.engagement.post.universal.slope = Normal.Distribution(0, log(4)/2),
    
    unsuppressed.disengagement.rates = Lognormal.Distribution(log(0.1392621), log(4)/2),
    suppressed.disengagement.rates = Lognormal.Distribution(log(0.1025866), log(4)/2),
    
    suppression.rate.0 = Lognormal.Distribution(log(1.118678), log(4)/2), # 12/23/2024: corrected this from probability (0.6732885) to rate 
    suppression.rate.1 = Lognormal.Distribution(log(1.118678), log(4)/2), # 12/23/2024: corrected this from probability (0.6732885) to rate 
    unsuppression.rates = Lognormal.Distribution(log(0.2196), log(4)/2), # 12/23/2024: corrected this from probability (0.1971601) to rate 
    
    male.awareness.multiplier = Lognormal.Distribution(log(1), log(4)/2),
    male.engagement.multiplier = Lognormal.Distribution(log(1), log(4)/2),
    male.suppression.multiplier = Lognormal.Distribution(log(1), log(4)/2),
    
    # mortality/fertility parameters 
    age.45.to.65.mortality.intercept.multiplier = Lognormal.Distribution(log(1), log(4)/2),
    age.45.to.65.mortality.slope.multiplier = Lognormal.Distribution(log(1), log(4)/2),
    over.65.mortality.intercept.multiplier = Lognormal.Distribution(log(1), log(4)/2),
    over.65.mortality.slope.multiplier = Lognormal.Distribution(log(1), log(4)/2),
    
    hiv.specific.mortality.rates.0 = Lognormal.Distribution(log(0.04), log(4)/2),
    hiv.specific.mortality.rates.1 = Lognormal.Distribution(log(0.07), log(4)/2),
    hiv.specific.mortality.rates.2 = Lognormal.Distribution(log(0.018), log(4)/2),
    
    male.hiv.mortality.multiplier.0 = Lognormal.Distribution(log(1), log(4)/2),
    male.hiv.mortality.multiplier.1 = Lognormal.Distribution(log(1), log(4)/2),
    male.hiv.mortality.multiplier.2 = Lognormal.Distribution(log(1), log(4)/2),
    
    age.0.to.14.hiv.mortality.multiplier.0 = Lognormal.Distribution(log(1), log(4)/2),
    age.0.to.14.hiv.mortality.multiplier.1 = Lognormal.Distribution(log(1), log(4)/2),
    age.0.to.14.hiv.mortality.multiplier.2 = Lognormal.Distribution(log(1), log(4)/2),
    
    age.15.to.24.hiv.mortality.multiplier.0 = Lognormal.Distribution(log(1), log(4)/2),
    age.15.to.24.hiv.mortality.multiplier.1 = Lognormal.Distribution(log(1), log(4)/2),
    age.15.to.24.hiv.mortality.multiplier.2 = Lognormal.Distribution(log(1), log(4)/2),
    
    over.50.hiv.mortality.multiplier.0 = Lognormal.Distribution(log(1), log(4)/2),
    over.50.hiv.mortality.multiplier.1 = Lognormal.Distribution(log(1), log(4)/2),
    over.50.hiv.mortality.multiplier.2 = Lognormal.Distribution(log(1), log(4)/2),
    
    fertility.multiplier = Lognormal.Distribution(log(1), log(4)/2),
    
    age.15.to.19.base.aging.rate=Lognormal.Distribution(log(0.25), log(4)/2),
    age.20.to.24.base.aging.rate=Lognormal.Distribution(log(0.25), log(4)/2),
    age.15.to.19.aging.factor=Lognormal.Distribution(log(2), log(4)/2),
    age.20.to.24.aging.factor=Lognormal.Distribution(log(2), log(4)/2),
    age.25.to.50.aging.factor=Lognormal.Distribution(log(2), log(4)/2),
    over.50.aging.factor=Lognormal.Distribution(log(1), log(2)/2) # changed 1/17/25 from log(2), log(4)/2
    
)
