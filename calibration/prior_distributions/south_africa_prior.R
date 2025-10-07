source('calibration/make_joint_distribution.R')

SOUTH.AFRICA.PRIOR = join.distributions(
    
    # general 
    trate.0 = Lognormal.Distribution(log(.4), log(4)/2),
    trate.1 = Lognormal.Distribution(log(.1), log(4)/2), 
    trate.2 = Lognormal.Distribution(log(.1), log(4)/2), 
    trate.3 = Lognormal.Distribution(log(.1), log(4)/2), 
    trate.4 = Lognormal.Distribution(log(.1), log(4)/2), 

    # COUNTRY-SPECIFIC; MUST EDIT:
    unsuppressed.disengagement.rates = Lognormal.Distribution(log(0.1554849), log(4)/2),
    suppressed.disengagement.rates = Lognormal.Distribution(log(0.1554849), log(4)/2),
    unsuppression.rates = Lognormal.Distribution(log(0.07548439), log(4)/2), 
    hiv.specific.mortality.rates.0 = Lognormal.Distribution(log(0.03030303), log(4)/2), # see hiv.mortality.priors.R
    hiv.specific.mortality.rates.1 = Lognormal.Distribution(log(0.05306122), log(4)/2),
    hiv.specific.mortality.rates.2 = Lognormal.Distribution(log(0.007432432), log(4)/2),
    birth.transmission.risk.0 = Logitnormal.Distribution(logit(0.42), log(3)/2), 
    birth.transmission.risk.1 = Logitnormal.Distribution(logit(0.3), log(3)/2), 
    # see Kenya prior for notes on a Logitnormal distribution 
    
    # NEW FOR YOUTH CASCADE CALIBRATION: 
    age.0.14.awareness.multiplier = Lognormal.Distribution(log(1), log(8)/2), 
    age.0.14.engagement.multiplier = Lognormal.Distribution(log(1), log(8)/2), 
    age.0.14.suppression.multiplier = Lognormal.Distribution(log(1), log(8)/2), # used the same as the male cascade multipliers since those are applied the same way 
    log.OR.0.14.engagement.slope = Normal.Distribution(0, log(4)/2),
    log.OR.0.14.suppression.slope = Normal.Distribution(0, log(4)/2),
    
    # THE REST OF THESE DON'T CHANGE 
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
    
    # cascade parameters
    log.OR.testing.intercept = Normal.Distribution(0, log(4)/2), 
    log.OR.testing.slope = Normal.Distribution(0, log(4)/2),
    
    log.OR.engagement.slope = Normal.Distribution(0, log(4)/2),

    log.OR.suppression.slope = Normal.Distribution(0, log(4)/2),

    male.awareness.multiplier = Lognormal.Distribution(log(1), log(8)/2),
    male.engagement.multiplier = Lognormal.Distribution(log(1), log(8)/2),
    male.suppression.multiplier = Lognormal.Distribution(log(1), log(8)/2),
    
    # mortality/fertility parameters 
    age.45.to.64.mortality.intercept.multiplier.male = Lognormal.Distribution(log(1), log(4)/2),
    age.45.to.64.mortality.slope.multiplier.male = Lognormal.Distribution(log(1), log(4)/2),
    age.65.to.79.mortality.intercept.multiplier.male = Lognormal.Distribution(log(1), log(4)/2),
    age.65.to.79.mortality.slope.multiplier.male = Lognormal.Distribution(log(1), log(4)/2),
    over.80.mortality.intercept.multiplier.male = Lognormal.Distribution(log(1), log(4)/2),
    over.80.mortality.slope.multiplier.male = Lognormal.Distribution(log(1), log(4)/2),
    
    age.45.to.64.mortality.intercept.multiplier.female = Lognormal.Distribution(log(1), log(4)/2),
    age.45.to.64.mortality.slope.multiplier.female = Lognormal.Distribution(log(1), log(4)/2),
    age.65.to.79.mortality.intercept.multiplier.female = Lognormal.Distribution(log(1), log(4)/2),
    age.65.to.79.mortality.slope.multiplier.female = Lognormal.Distribution(log(1), log(4)/2),
    over.80.mortality.intercept.multiplier.female = Lognormal.Distribution(log(1), log(4)/2),
    over.80.mortality.slope.multiplier.female = Lognormal.Distribution(log(1), log(4)/2),
    
    male.hiv.mortality.multiplier.0 = Lognormal.Distribution(log(1), log(4)/2),
    male.hiv.mortality.multiplier.1 = Lognormal.Distribution(log(1), log(4)/2),
    male.hiv.mortality.multiplier.2 = Lognormal.Distribution(log(1), log(4)/2),
    
    age.0.to.4.hiv.mortality.multiplier.0 = Lognormal.Distribution(log(1), log(4)/2),
    age.5.to.14.hiv.mortality.multiplier.0 = Lognormal.Distribution(log(1), log(4)/2),
    age.0.to.14.hiv.mortality.multiplier.1 = Lognormal.Distribution(log(1), log(4)/2),
    age.0.to.14.hiv.mortality.multiplier.2 = Lognormal.Distribution(log(1), log(4)/2),
    
    age.15.to.24.hiv.mortality.multiplier.0 = Lognormal.Distribution(log(1), log(4)/2),
    age.15.to.24.hiv.mortality.multiplier.1 = Lognormal.Distribution(log(1), log(4)/2),
    age.15.to.24.hiv.mortality.multiplier.2 = Lognormal.Distribution(log(1), log(4)/2),
    
    age.25.to.49.hiv.mortality.multiplier.0 = Lognormal.Distribution(log(1), log(4)/2),
    age.25.to.49.hiv.mortality.multiplier.1 = Lognormal.Distribution(log(1), log(4)/2),
    age.25.to.49.hiv.mortality.multiplier.2 = Lognormal.Distribution(log(1), log(4)/2),
    
    over.50.hiv.mortality.multiplier.0 = Lognormal.Distribution(log(1), log(4)/2),
    over.50.hiv.mortality.multiplier.1 = Lognormal.Distribution(log(1), log(4)/2),
    over.50.hiv.mortality.multiplier.2 = Lognormal.Distribution(log(1), log(4)/2),
    
    fertility.multiplier = Lognormal.Distribution(log(1), log(4)/2),
    
    age.15.to.19.base.aging.rate=Lognormal.Distribution(log(0.25), log(4)/2),
    age.20.to.24.base.aging.rate=Lognormal.Distribution(log(0.25), log(4)/2),
    age.15.to.19.aging.factor=Lognormal.Distribution(log(2), log(4)/2),
    age.20.to.24.aging.factor=Lognormal.Distribution(log(2), log(4)/2),
    age.25.to.50.aging.factor=Lognormal.Distribution(log(2), log(4)/2)# ,
    # over.50.aging.factor=Lognormal.Distribution(log(1), log(1.5)/2) # removed 2/20 
    # changed 2/18 to log(1.5) from log(2) - (1/17), log(4)/2 - original
    
)
