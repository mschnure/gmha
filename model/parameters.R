#################################################################################################
# Description: Functions to set up the parameter object with constant and time-varying parameters
#################################################################################################

# Core functions 
#     1. create.model.parameters 
#     2. get.default.parameters 
#     3. map.model.parameters 
#     4. add.time.varying.parameter.value
#     5. compute.time.varying.parameters
# Other/helper functions 
#     1. make.transmission.array


library(splines)

##--------------------##
##-- CORE FUNCTIONS --##
##--------------------##

# Sets up the basic parameters (sexes, ages, subgroups, HIV status); 
# call this function with no arguments when first setting up a test case 
create.model.parameters <- function(location,
                                    age.cutoffs=MODEL.AGE.CUTOFFS, #the lower limit for each bracket
                                    sexes = c('male','female'),
                                    subgroups = 'all',
                                    min.sexually.active.age=14){
    parameters = list()
    
    #-- SET UP THE BASICS --#
    
    # sex, risk, subpop
    parameters$SEXES = sexes 
    parameters$SUBGROUPS = subgroups
    
    # ages
    parsed.ages = parse.age.brackets(age.cutoffs)
    
    parameters$AGES = parsed.ages$labels
    parameters$AGE.SPANS = parsed.ages$spans
    parameters$AGE.LOWERS = parsed.ages$lowers
    names(parameters$AGE.LOWERS) = parameters$AGES
    parameters$AGE.UPPERS = parsed.ages$uppers
    names(parameters$AGE.UPPERS) = parameters$AGES
    
    
    # hiv status
    parameters$HIV.STATUS = c('hiv_negative','undiagnosed','diagnosed_unengaged','engaged_unsuppressed','engaged_suppressed')
    parameters$HIV.STATES = c('undiagnosed','diagnosed_unengaged','engaged_unsuppressed','engaged_suppressed') 
    parameters$DIAGNOSED.STATES = c('diagnosed_unengaged','engaged_unsuppressed','engaged_suppressed')
    parameters$ENGAGED.STATES = c('engaged_unsuppressed','engaged_suppressed')
    
    parameters$min.sexually.active.age = min.sexually.active.age
    parameters$male.to.female.age.model = get.male.to.female.age.model(location = location)
    parameters$female.to.male.age.model = get.female.to.male.age.model(location = location)
    
    #- Return --#
    parameters  
}

# Sets default values for parameters we will sample; 
# called in “run_systematic” code with the option to change values
get.default.parameters = function(location){
    
    # if(location!="Kenya")
    #     print("Using Kenya-specific parameters for now; need to update with other locations")
    
    rv = c(
        ## Transmission parameters ##
        # general
        start.time=1975,
        time.0=1990, # 6/12 added this to sampled parameters
        time.1=1997, # 6/12 added this to sampled parameters
        time.2=2008, # 6/12 added this to sampled parameters
        time.3=2018, # 6/12 added this to sampled parameters
        time.3.5=2030,
        time.4=2040,
        trate.0=0.8, # kenya prior; others set below 
        trate.1=0.1, # kenya prior
        trate.2=0.1, # kenya prior 
        trate.3=0.1, # kenya prior
        trate.4=0.1, # kenya prior
        proportion.trate.change.by.3.5=0.75,
        # sex transmission multipliers
        male.to.male.multiplier=1,
        female.to.male.multiplier=1,
        # age transmission multipliers
        age.15.to.19.transmission.multiplier.0=1,
        age.15.to.19.transmission.multiplier.1=1,
        age.15.to.19.transmission.multiplier.2=1,
        age.15.to.19.transmission.multiplier.3=1,
        
        age.20.to.29.transmission.multiplier.0=1,
        age.20.to.29.transmission.multiplier.1=1,
        age.20.to.29.transmission.multiplier.2=1,
        age.20.to.29.transmission.multiplier.3=1,
        
        age.40.to.49.transmission.multiplier.0=1,
        age.40.to.49.transmission.multiplier.1=1,
        age.40.to.49.transmission.multiplier.2=1,
        age.40.to.49.transmission.multiplier.3=1,
        
        age.50.and.over.transmission.multiplier.0=1,
        age.50.and.over.transmission.multiplier.1=1,
        age.50.and.over.transmission.multiplier.2=1,
        age.50.and.over.transmission.multiplier.3=1,
        # other transmission multipliers
        relative.transmission.from.diagnosis=0.33, 
        age.assortativity=1, 
        birth.transmission.time.0=1990,
        birth.transmission.time.1=2020,
        birth.transmission.risk.0=0.42,
        birth.transmission.risk.1=0.30,
        
        ## Cascade parameters ##
        log.OR.testing.intercept=0, # 0 because on log scale
        log.OR.testing.slope=0,
        log.OR.engagement.slope=0,
        unsuppressed.disengagement.rates=0.1392621, # KENYA value, updated below 
        suppressed.disengagement.rates = 0.1025866, # KENYA value, updated below 
        log.OR.suppression.slope=0,
        unsuppression.rates=0.2196, # KENYA value, using for all except SA for now
        male.awareness.multiplier=1,
        male.engagement.multiplier=1,
        male.suppression.multiplier=1,
        cascade.improvement.end.year=2040,
        
        ## Mortality/fertility parameters ##
        # multiplies intercept or slope before projecting
        # general mortality 
        age.45.to.64.mortality.intercept.multiplier.male=1,
        age.45.to.64.mortality.intercept.multiplier.female=1,
        age.45.to.64.mortality.slope.multiplier.male=1,
        age.45.to.64.mortality.slope.multiplier.female=1,
        
        age.65.to.79.mortality.intercept.multiplier.male=1,
        age.65.to.79.mortality.intercept.multiplier.female=1,
        age.65.to.79.mortality.slope.multiplier.male=1,
        age.65.to.79.mortality.slope.multiplier.female=1,
        
        over.80.mortality.intercept.multiplier.male=1,
        over.80.mortality.intercept.multiplier.female=1,
        over.80.mortality.slope.multiplier.male=1, 
        over.80.mortality.slope.multiplier.female=1, 
        
        # hiv mortality 
        hiv.mortality.time.0=1990,
        hiv.mortality.time.1=2005,
        hiv.mortality.time.2=2020,
        hiv.specific.mortality.rates.0=HIV.MORTALITY.PRIORS[[location]][["1990"]], 
        hiv.specific.mortality.rates.1=HIV.MORTALITY.PRIORS[[location]][["2005"]], 
        hiv.specific.mortality.rates.2=HIV.MORTALITY.PRIORS[[location]][["2020"]], 
        male.hiv.mortality.multiplier.0=1,
        male.hiv.mortality.multiplier.1=1,
        male.hiv.mortality.multiplier.2=1,
        
        age.0.to.4.hiv.mortality.multiplier.0=1, 
        age.5.to.14.hiv.mortality.multiplier.0=1,
        age.0.to.14.hiv.mortality.multiplier.1=1,
        age.0.to.14.hiv.mortality.multiplier.2=1,
        age.15.to.24.hiv.mortality.multiplier.0=1,
        age.15.to.24.hiv.mortality.multiplier.1=1,
        age.15.to.24.hiv.mortality.multiplier.2=1,
        age.25.to.49.hiv.mortality.multiplier.0=1,
        age.25.to.49.hiv.mortality.multiplier.1=1,
        age.25.to.49.hiv.mortality.multiplier.2=1,
        over.50.hiv.mortality.multiplier.0=1,
        over.50.hiv.mortality.multiplier.1=1,
        over.50.hiv.mortality.multiplier.2=1,
        fertility.multiplier=1,
        
        # Aging rates
        age.15.to.19.base.aging.rate=0.25,
        age.20.to.24.base.aging.rate=0.25,
        age.15.to.19.aging.factor=2,
        age.20.to.24.aging.factor=2,
        age.25.to.50.aging.factor=2,
        over.50.aging.factor=1 
    ) 
    
    if(location=="South Africa"){
        rv["trate.0"] = 0.4
        rv["trate.1"] = 0.1
        rv["trate.2"] = 0.1
        rv["trate.3"] = 0.1
        rv["trate.4"] = 0.1
        rv["unsuppressed.disengagement.rates"]= 0.1554849 # see disengagement models
        rv["suppressed.disengagement.rates"] = 0.1554849 # see disengagement models
        rv["unsuppression.rates"] = 0.07548439 
    }  else if(location=="non.unaids.remainder"){
        rv["unsuppressed.disengagement.rates"]= 0.334341 
        rv["suppressed.disengagement.rates"] = 0.334341 
        rv["unsuppression.rates"] = 0.06238609   
    } else if(location=="France"){
        rv["trate.0"] = 0.5
        rv["trate.1"] = 0.12# 0.1, 1997
        rv["trate.2"] = 0.12 # 0.1, 2008
        rv["trate.3"] = 0.15 # 0.1, 2018
        rv["trate.4"] = 0.15 # 0.1, 2030? 
        rv["birth.transmission.risk.0"]=0.141
        rv["unsuppressed.disengagement.rates"]= 0.0965109
        rv["suppressed.disengagement.rates"] = 0.0965109
        rv["unsuppression.rates"] = 0.09431068   
    } else if(location=="Nigeria"){
        rv["trate.0"] = 0.65
        rv["trate.1"] = 0.1
        rv["trate.2"] = 0.1
        rv["trate.3"] = 0.1
        rv["trate.4"] = 0.1
    } else if(location=="Thailand"){
        rv["trate.0"] = 0.65
        rv["trate.1"] = 0.06
        rv["trate.2"] = 0.06
        rv["trate.3"] = 0.06
        rv["trate.4"] = 0.06
        rv["unsuppressed.disengagement.rates"] = 0.1731636
        rv["suppressed.disengagement.rates"] = 0.1731636 
        rv["unsuppression.rates"] = 0.0311 
        rv["birth.transmission.risk.0"]=0.242
    } else if(location=="Cambodia"){
        rv["trate.0"] = 0.47
        rv["trate.1"] = 0.06
        rv["trate.2"] = 0.06
        rv["trate.3"] = 0.06
        rv["trate.4"] = 0.06
        rv["unsuppressed.disengagement.rates"] = 0.0618754 
        rv["suppressed.disengagement.rates"] = 0.0618754 
        rv["unsuppression.rates"] = 0.0618754 
        rv["birth.transmission.risk.0"]=0.305
    }  else { # if (location=="Kenya")
        rv["hiv.specific.mortality.rates.0"]=0.04057971 # Kenya
        rv["hiv.specific.mortality.rates.1"]=0.08125 # Kenya 
        rv["hiv.specific.mortality.rates.2"]=0.02 # Kenya 
        rv["unsuppressed.disengagement.rates"] = 0.1392621 # Kenya's value 
        rv["suppressed.disengagement.rates"] = 0.1025866 # Kenya's value 
        rv["unsuppression.rates"] = 0.2196 # Kenya's value 
    } 
    
    rv
      
}



#-- MAP ALL PARAMETERS --#
#   1. Uses parameters object (set up via create.model.parameters) and sampled parameters 
#       (set up via get.default.parameters) to set up full set of parameters needed for diffeq 
#       (all dimensions of age/sex, etc., all years) 
#   2. Types of parameters: fertility, aging, mortality (HIV/non-HIV), diagnoses, transmission rates, 
#       infectiousness, engagement/disengagement, suppression/unsuppression
#   3. Everything technically added as a time-varying parameter even if it doesn’t vary 
map.model.parameters <- function(parameters,
                                 location,
                                 sampled.parameters=get.default.parameters(location=location),
                                 age.cutoffs=MODEL.AGE.CUTOFFS,
                                 project.to.year=2040,
                                 interventions=NO.INTERVENTION){
    
    if(is(interventions,"intervention")){
        interventions = list(interventions)
    }
    
    #-- SET UP DIMENSIONS --#
    state.dim.names = list(age=parameters$AGES, 
                           sex=parameters$SEXES,
                           subgroup=parameters$SUBGROUPS,
                           hiv.status=parameters$HIV.STATUS)
    
    n.states = prod(sapply(state.dim.names, length))
    
    trans.dim.names = list(age=parameters$AGES, 
                           sex=parameters$SEXES,
                           subgroup=parameters$SUBGROUPS)
    
    n.trans.states = prod(sapply(trans.dim.names, length))
    
    
    
    #-- BIRTH --# 
    age.specific.fertility = get.surveillance.data(data.manager = DATA.MANAGER, 
                                                   data.type = "fertility", 
                                                   location = location,
                                                   years = DATA.MANAGER$fertility$YEARS, 
                                                   keep.dimensions = c('year','age','location'))
    
    full.dim.names = list(year = DATA.MANAGER$fertility$YEARS,
                          age = parameters$AGES,
                          sex = parameters$SEXES)
    
    all.fertility = array(0,
                          dim = sapply(full.dim.names, length),
                          dimnames = full.dim.names)
    
    all.fertility[,dimnames(age.specific.fertility)$age,"female"] = age.specific.fertility
    
    for (year in dimnames(all.fertility)$year){
        rv = array(all.fertility[year,,]*sampled.parameters["fertility.multiplier"], #added a tuning parameter to match population
                   dim = sapply(state.dim.names, length),
                   dimnames = state.dim.names)
        
        parameters = add.time.varying.parameter.value(parameters,
                                                      parameter.name='FERTILITY.RATES',
                                                      value = rv,
                                                      time = as.numeric(year))
    }
    
    parameters = add.time.varying.parameter.value(parameters,
                                                  parameter.name='MALE.BIRTHS',
                                                  value = 0.5,
                                                  time = 2000)
    
    
    
    #-- AGING --#
    base.aging.rates = array((1/parameters$AGE.SPANS),
                        dim=sapply(state.dim.names, length),
                        dimnames=state.dim.names)
    
    # allow 15-19 and 20-24 to have different/higher base aging rates,
    # because we assume most of the HIV infections in this age group are among those on the older side who are sexually active
    # (and therefore will increase the proportion aging out )
    base.aging.rates["15-19",,,] = sampled.parameters["age.15.to.19.base.aging.rate"] 
    base.aging.rates["20-24",,,] = sampled.parameters["age.20.to.24.base.aging.rate"]
    
    get.aging.rates = function(times,
                               base.rate, # base aging rate, i.e., 0.2
                               factor, # factor to either divide or multiply the aging rate by
                               pre.time, # a few years before the low time
                               low.time, # when aging rate in that age span is lowest; i.e., when bulk of the age span is youngest and not aging out
                               high.time, # when aging rate in that age span is highest; i.e., when bulk of the age span is oldest and aging out
                               post.time) # a few years after the high time 
    {
        rv = exp(spline(x=c(pre.time, low.time, high.time, post.time),
                    y=log(c(base.rate, base.rate/factor, base.rate*factor, base.rate)),
                    method='natural',
                    xout = times)$y)
        rv[times<pre.time | times>post.time] = base.rate
        
        rv
    }
    
    aging.years=1980:2030
    low.time.for.age = c("15-19" = 1992,"20-24" = 1996,"25-29" = 2000,"30-34" = 2004,"35-39" = 2008,"40-44" = 2012,
                         "45-49" = 2016,"50-54" = 2020,"55-59" = 2024,"60-64" = 2028,"65-69" = 2032,"70-74" = 2036,
                         "75-79" = 2040)
    
    age.brackets.to.update = parameters$AGES[c(-1,-2,-3)]
    age.brackets.to.update = age.brackets.to.update[-length(age.brackets.to.update)] # remove 80 and over
    
    rates.per.age = lapply(age.brackets.to.update, function(age){
        age.span=5
        low.time = low.time.for.age[age]
        high.time = low.time + age.span-1 
        pre.time = low.time-3
        post.time = high.time + 3
        
        # Get base rate
        base.rate = 1/age.span
        if (age=='15-19')
            base.rate = sampled.parameters['age.15.to.19.base.aging.rate']
        else if (age=='20-24')
            base.rate = sampled.parameters['age.20.to.24.base.aging.rate']
        
        # Get factor by age
        factor=2
        age.25.to.50.age.brackets = get.age.brackets.in.range(lower = 25, 
                                                              upper = 50) 
        over.50.age.brackets = get.age.brackets.in.range(lower = 50, 
                                                         upper = Inf) 
        
        if(age=="15-19")
            factor=sampled.parameters['age.15.to.19.aging.factor']
        else if (age=="20-24")
            factor=sampled.parameters['age.20.to.24.aging.factor']
        else if (age %in% age.25.to.50.age.brackets)
            factor=sampled.parameters['age.25.to.50.aging.factor']
        else if (age %in% over.50.age.brackets)
            factor=sampled.parameters['over.50.aging.factor']

        get.aging.rates(times=aging.years,
                        base.rate=base.rate,
                        factor=factor,
                        pre.time=pre.time,
                        low.time=low.time,
                        high.time=high.time,
                        post.time=post.time)
    })

    for (year in 1:length(aging.years)) {
        aging.rates = base.aging.rates
        for (age in 1:length(age.brackets.to.update)){
            age.name = age.brackets.to.update[age]
            aging.rates[age.name,,,setdiff(dimnames(aging.rates)$hiv.status,"hiv_negative")] = rates.per.age[[age]][year] }
        
        parameters = add.time.varying.parameter.value(parameters,
                                                      parameter.name='AGING.RATES',
                                                      value = aging.rates,
                                                      time = aging.years[year])
    }
    
    #-- MORTALITY --#
    ## HIV MORTALITY ## 
    age.0.to.14.age.brackets = get.age.brackets.in.range(lower = 0, 
                                                         upper = 15) 
    age.5.to.14.age.brackets = get.age.brackets.in.range(lower = 5, 
                                                         upper = 15) 
    age.15.to.24.age.brackets = get.age.brackets.in.range(lower = 15, 
                                                         upper = 25) 
    age.25.to.49.age.brackets = get.age.brackets.in.range(lower = 25, 
                                                          upper = 50) 
    over.50.age.brackets = get.age.brackets.in.range(lower = 50, 
                                                    upper = Inf) 
    # Set up initial HIV mortality rates 
    HIV.MORTALITY.RATES.0=array(0, #assume 0 for non-hiv states
                              dim=sapply(state.dim.names, length),
                              dimnames=state.dim.names)
    HIV.MORTALITY.RATES.2 = HIV.MORTALITY.RATES.1 = HIV.MORTALITY.RATES.0
    
    # Set up time-specific HIV mortality rates 
    HIV.MORTALITY.RATES.0[,,,c('undiagnosed', 'diagnosed_unengaged', 'engaged_unsuppressed')] = 
        sampled.parameters['hiv.specific.mortality.rates.0']
    HIV.MORTALITY.RATES.0[,"male",,] = HIV.MORTALITY.RATES.0[,"male",,]*sampled.parameters["male.hiv.mortality.multiplier.0"]
    
    HIV.MORTALITY.RATES.1[,,,c('undiagnosed', 'diagnosed_unengaged', 'engaged_unsuppressed')] = 
        sampled.parameters['hiv.specific.mortality.rates.1']
    HIV.MORTALITY.RATES.1[,"male",,] = HIV.MORTALITY.RATES.1[,"male",,]*sampled.parameters["male.hiv.mortality.multiplier.1"]
    
    HIV.MORTALITY.RATES.2[,,,c('undiagnosed', 'diagnosed_unengaged', 'engaged_unsuppressed')] = 
        sampled.parameters['hiv.specific.mortality.rates.2']
    HIV.MORTALITY.RATES.2[,"male",,] = HIV.MORTALITY.RATES.2[,"male",,]*sampled.parameters["male.hiv.mortality.multiplier.2"]
    
    # Set up age- and time-specific HIV mortality multipliers 
    HIV.MORTALITY.RATES.0["0-4",,,] = HIV.MORTALITY.RATES.0["0-4",,,]*sampled.parameters["age.0.to.4.hiv.mortality.multiplier.0"]
    HIV.MORTALITY.RATES.0[age.5.to.14.age.brackets,,,] = HIV.MORTALITY.RATES.0[age.5.to.14.age.brackets,,,]*
        sampled.parameters["age.5.to.14.hiv.mortality.multiplier.0"]
    HIV.MORTALITY.RATES.1[age.0.to.14.age.brackets,,,] = HIV.MORTALITY.RATES.1[age.0.to.14.age.brackets,,,]*
        sampled.parameters["age.0.to.14.hiv.mortality.multiplier.1"]
    HIV.MORTALITY.RATES.2[age.0.to.14.age.brackets,,,] = HIV.MORTALITY.RATES.2[age.0.to.14.age.brackets,,,]*
        sampled.parameters["age.0.to.14.hiv.mortality.multiplier.2"]
    
    HIV.MORTALITY.RATES.0[age.15.to.24.age.brackets,,,] = HIV.MORTALITY.RATES.0[age.15.to.24.age.brackets,,,]*
        sampled.parameters["age.15.to.24.hiv.mortality.multiplier.0"]
    HIV.MORTALITY.RATES.1[age.15.to.24.age.brackets,,,] = HIV.MORTALITY.RATES.1[age.15.to.24.age.brackets,,,]*
        sampled.parameters["age.15.to.24.hiv.mortality.multiplier.1"]
    HIV.MORTALITY.RATES.2[age.15.to.24.age.brackets,,,] = HIV.MORTALITY.RATES.2[age.15.to.24.age.brackets,,,]*
        sampled.parameters["age.15.to.24.hiv.mortality.multiplier.2"]
    
    HIV.MORTALITY.RATES.0[age.25.to.49.age.brackets,,,] = HIV.MORTALITY.RATES.0[age.25.to.49.age.brackets,,,]*
        sampled.parameters["age.25.to.49.hiv.mortality.multiplier.0"]
    HIV.MORTALITY.RATES.1[age.25.to.49.age.brackets,,,] = HIV.MORTALITY.RATES.1[age.25.to.49.age.brackets,,,]*
        sampled.parameters["age.25.to.49.hiv.mortality.multiplier.1"]
    HIV.MORTALITY.RATES.2[age.25.to.49.age.brackets,,,] = HIV.MORTALITY.RATES.2[age.25.to.49.age.brackets,,,]*
        sampled.parameters["age.25.to.49.hiv.mortality.multiplier.2"]
    
    HIV.MORTALITY.RATES.0[over.50.age.brackets,,,] = HIV.MORTALITY.RATES.0[over.50.age.brackets,,,]*
        sampled.parameters["over.50.hiv.mortality.multiplier.0"]
    HIV.MORTALITY.RATES.1[over.50.age.brackets,,,] = HIV.MORTALITY.RATES.1[over.50.age.brackets,,,]*
        sampled.parameters["over.50.hiv.mortality.multiplier.1"]
    HIV.MORTALITY.RATES.2[over.50.age.brackets,,,] = HIV.MORTALITY.RATES.2[over.50.age.brackets,,,]*
        sampled.parameters["over.50.hiv.mortality.multiplier.2"]
    
    parameters = add.time.varying.parameter.value(parameters,
                                                  parameter.name='HIV.MORTALITY.RATES',
                                                  value = HIV.MORTALITY.RATES.0,
                                                  time = sampled.parameters['hiv.mortality.time.0'])
    parameters = add.time.varying.parameter.value(parameters,
                                                  parameter.name='HIV.MORTALITY.RATES',
                                                  value = HIV.MORTALITY.RATES.1,
                                                  time = sampled.parameters['hiv.mortality.time.1'])
    parameters = add.time.varying.parameter.value(parameters,
                                                  parameter.name='HIV.MORTALITY.RATES',
                                                  value = HIV.MORTALITY.RATES.2,
                                                  time = sampled.parameters['hiv.mortality.time.2'])
    
    
    ## NON-HIV MORTALITY ## 
    # see calculate_death_rates.R
    smooth.deaths.age.sex = project.deaths(data.manager = DATA.MANAGER,
                                           location = location,
                                           project.to.year = project.to.year,
                                           keep.dimensions = c('year','sex','age','location'),
                                           model.age.cutoffs = MODEL.AGE.CUTOFFS,
                                           sampled.parameters = sampled.parameters)
    
    for (year in dimnames(smooth.deaths.age.sex)$year){
        rv = array(smooth.deaths.age.sex[year,,],
                   dim = sapply(state.dim.names, length),
                   dimnames = state.dim.names)
        parameters = add.time.varying.parameter.value(parameters,
                                                      parameter.name='NON.HIV.MORTALITY.RATES',
                                                      value = rv,
                                                      time = as.numeric(year))
    }
    
    
    #-- DIAGNOSES --#
    testing.model = get.testing.model(location=location)
    
    testing.times = c(1976:min(project.to.year,sampled.parameters["cascade.improvement.end.year"], na.rm = T))
    testing.rates = c(lapply(testing.times, function(year){
    
        projected.log.odds = (testing.model$intercepts+sampled.parameters['log.OR.testing.intercept'])+
            ((testing.model$slopes+sampled.parameters['log.OR.testing.slope'])*(year-testing.model$anchor.year))
        
        projected.p = 1/(1+exp(-projected.log.odds))
        projected.p = projected.p*testing.model$max.proportion
        projected.rate = -log(1-projected.p)
        projected.rate[,"male",] = projected.rate[,"male",]*sampled.parameters["male.awareness.multiplier"]
        
        projected.rate
        
    }))
    
    testing.times = c(sampled.parameters["start.time"],testing.times)
    testing.rates = c(list(array(0,
                                 dim=dim(testing.rates[[1]]),
                                 dimnames = dimnames(testing.rates[[1]]))),
                      testing.rates)
    
    parameters = set.rates.for.interventions(baseline.rates = testing.rates, # list 
                                             baseline.times = testing.times, # vector
                                             interventions = interventions,
                                             scale = "rate", 
                                             parameters = parameters,
                                             parameter.name = "TESTING.RATES")
    
    
    #-- NEW INFECTIONS --#
    age.sex.trans.multipliers = get.all.age.sex.transmission.multipliers(location=location)
    
    transmission.dim.names = list(age.to=parameters$AGES, 
                                  sex.to=parameters$SEXES,
                                  subgroup.to=parameters$SUBGROUPS,
                                  age.from=parameters$AGES, 
                                  sex.from=parameters$SEXES,
                                  subgroup.from=parameters$SUBGROUPS)
    
    # Mixing proportions: array where every combo of age.to, sex.to sums to 1 (capturing all of their partners)
    # This is country-specific, defined in parameters$male.to.female.age.model and parameters$female.to.male.age.model above 
    mixing.proportions.0 = sapply(parameters$SEXES, function(sex.to){
        sapply(parameters$AGES, function(age.to){
            # if(age.to=="10-14")
            #     browser()
            # for this age bracket/sex.to, what proportion of that sexes partners are in the other sexes
            sex.proportions = get.sex.mixing.proportions(sex.to = sex.to,
                                                         age.to=age.to,
                                                         sexes=parameters$SEXES,
                                                         sampled.parameters = sampled.parameters)      
            
            # then, for that sex-sex combination what proportion are in each age group
            sapply(parameters$SEXES, function(sex.from){
                sex.proportions[sex.from]*get.age.mixing.proportions(parameters=parameters,
                                                                     sex.to=sex.to,
                                                                     age.to=age.to,
                                                                     sex.from=sex.from,
                                                                     ages=parameters$AGES,
                                                                     sampled.parameters=sampled.parameters)
            })
        })
    })
    
    dim(mixing.proportions.0) = c(n.trans.states, n.trans.states)
    mixing.proportions.0 = t(mixing.proportions.0)
    dim(mixing.proportions.0) = sapply(transmission.dim.names, length)
    dimnames(mixing.proportions.0) = transmission.dim.names
    
    # Set transmission to 0 until start.time 
    parameters = add.time.varying.parameter.value(parameters,
                                                  parameter.name='TRANSMISSION.RATES',
                                                  value = 0,
                                                  time = (sampled.parameters['start.time']-0.001))
    
    # Set transmission rate to a high level (trate.0) at start.time, don't use age multipliers
    transmission.rates.0 = make.transmission.array(parameters = parameters,
                                                   global.trate = sampled.parameters["trate.0"],
                                                   male.to.male.multiplier = sampled.parameters["male.to.male.multiplier"], 
                                                   female.to.male.multiplier = sampled.parameters["female.to.male.multiplier"],
                                                   female.age.multipliers =c(rep(0,length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 0, upper = 15))),
                                                                             rep(sampled.parameters["age.15.to.19.transmission.multiplier.0"],
                                                                                 length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 15, upper = 20))),
                                                                             rep(sampled.parameters["age.20.to.29.transmission.multiplier.0"],
                                                                                 length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 20, upper = 30))),
                                                                             rep(1,length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 30, upper = 40))),
                                                                             rep(sampled.parameters["age.40.to.49.transmission.multiplier.0"],
                                                                                 length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 40, upper = 50))),
                                                                             rep(sampled.parameters["age.50.and.over.transmission.multiplier.0"],
                                                                                 length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 50, upper = Inf)))),
                                                   male.age.multipliers =c(rep(0,length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 0, upper = 15))),
                                                                           rep(sampled.parameters["age.15.to.19.transmission.multiplier.0"],
                                                                               length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 15, upper = 20))),
                                                                           rep(sampled.parameters["age.20.to.29.transmission.multiplier.0"],
                                                                               length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 20, upper = 30))),
                                                                           rep(1,length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 30, upper = 40))),
                                                                           rep(sampled.parameters["age.40.to.49.transmission.multiplier.0"],
                                                                               length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 40, upper = 50))),
                                                                           rep(sampled.parameters["age.50.and.over.transmission.multiplier.0"],
                                                                               length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 50, upper = Inf)))))

    transmission.rates.0=transmission.rates.0*mixing.proportions.0
    
    dim(transmission.rates.0) = c(n.trans.states, n.trans.states)
    transmission.rates.0 = as.matrix(transmission.rates.0)
    
    parameters = add.time.varying.parameter.value(parameters,
                                                  parameter.name='TRANSMISSION.RATES',
                                                  value = transmission.rates.0,
                                                  time = sampled.parameters['start.time'])
    
    # End high transmission rate at time.0 - have to set it again so that this is the year it interpolates from
    parameters = add.time.varying.parameter.value(parameters,
                                                  parameter.name='TRANSMISSION.RATES',
                                                  value = transmission.rates.0,
                                                  time = sampled.parameters['time.0'])
    
    # Set transmission rate to a lower level (trate.1) at time.1, use 2003 multipliers
    transmission.rates.1 = make.transmission.array(parameters = parameters,
                                                   global.trate = sampled.parameters["trate.1"],
                                                   male.to.male.multiplier = sampled.parameters["male.to.male.multiplier"], 
                                                   female.to.male.multiplier = sampled.parameters["female.to.male.multiplier"],
                                                   female.age.multipliers =c(rep(0,length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 0, upper = 15))),
                                                                             rep(sampled.parameters["age.15.to.19.transmission.multiplier.1"],
                                                                                 length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 15, upper = 20))),
                                                                             rep(sampled.parameters["age.20.to.29.transmission.multiplier.1"],
                                                                                 length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 20, upper = 30))),
                                                                             rep(1,length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 30, upper = 40))),
                                                                             rep(sampled.parameters["age.40.to.49.transmission.multiplier.1"],
                                                                                 length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 40, upper = 50))),
                                                                             rep(sampled.parameters["age.50.and.over.transmission.multiplier.1"],
                                                                                 length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 50, upper = Inf))))*
                                                       age.sex.trans.multipliers$FEMALE.AGE.MULTIPLIERS.2003,
                                                   male.age.multipliers =c(rep(0,length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 0, upper = 15))),
                                                                           rep(sampled.parameters["age.15.to.19.transmission.multiplier.1"],
                                                                               length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 15, upper = 20))),
                                                                           rep(sampled.parameters["age.20.to.29.transmission.multiplier.1"],
                                                                               length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 20, upper = 30))),
                                                                           rep(1,length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 30, upper = 40))),
                                                                           rep(sampled.parameters["age.40.to.49.transmission.multiplier.1"],
                                                                               length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 40, upper = 50))),
                                                                           rep(sampled.parameters["age.50.and.over.transmission.multiplier.1"],
                                                                               length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 50, upper = Inf))))*
                                                       age.sex.trans.multipliers$MALE.AGE.MULTIPLIERS.2003)
    
    mixing.proportions.1 = mixing.proportions.0 # keep this the same for now
    transmission.rates.1=transmission.rates.1*mixing.proportions.1
    
    dim(transmission.rates.1) = c(n.trans.states, n.trans.states)
    transmission.rates.1 = as.matrix(transmission.rates.1)
    
    parameters = add.time.varying.parameter.value(parameters,
                                                  parameter.name='TRANSMISSION.RATES',
                                                  value = transmission.rates.1,
                                                  time = sampled.parameters['time.1'])
    
    # Set transmission rate to another level (trate.2) at time.2
    transmission.rates.2 = make.transmission.array(parameters = parameters,
                                                   global.trate = sampled.parameters["trate.2"],
                                                   male.to.male.multiplier = sampled.parameters["male.to.male.multiplier"], 
                                                   female.to.male.multiplier = sampled.parameters["female.to.male.multiplier"],
                                                   female.age.multipliers =c(rep(0,length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 0, upper = 15))),
                                                                             rep(sampled.parameters["age.15.to.19.transmission.multiplier.2"],
                                                                                 length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 15, upper = 20))),
                                                                             rep(sampled.parameters["age.20.to.29.transmission.multiplier.2"],
                                                                                 length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 20, upper = 30))),
                                                                             rep(1,length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 30, upper = 40))),
                                                                             rep(sampled.parameters["age.40.to.49.transmission.multiplier.2"],
                                                                                 length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 40, upper = 50))),
                                                                             rep(sampled.parameters["age.50.and.over.transmission.multiplier.2"],
                                                                                 length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 50, upper = Inf))))*
                                                       age.sex.trans.multipliers$FEMALE.AGE.MULTIPLIERS.2008,
                                                   male.age.multipliers =c(rep(0,length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 0, upper = 15))),
                                                                           rep(sampled.parameters["age.15.to.19.transmission.multiplier.2"],
                                                                               length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 15, upper = 20))),
                                                                           rep(sampled.parameters["age.20.to.29.transmission.multiplier.2"],
                                                                               length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 20, upper = 30))),
                                                                           rep(1,length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 30, upper = 40))),
                                                                           rep(sampled.parameters["age.40.to.49.transmission.multiplier.2"],
                                                                               length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 40, upper = 50))),
                                                                           rep(sampled.parameters["age.50.and.over.transmission.multiplier.2"],
                                                                               length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 50, upper = Inf))))*
                                                       age.sex.trans.multipliers$MALE.AGE.MULTIPLIERS.2008)
    
    mixing.proportions.2 = mixing.proportions.0 # keep this the same for now
    transmission.rates.2=transmission.rates.2*mixing.proportions.2
    
    dim(transmission.rates.2) = c(n.trans.states, n.trans.states)
    transmission.rates.2 = as.matrix(transmission.rates.2)
    
    parameters = add.time.varying.parameter.value(parameters,
                                                  parameter.name='TRANSMISSION.RATES',
                                                  value = transmission.rates.2,
                                                  time = sampled.parameters['time.2'])
    
    # Set transmission rate to another level (trate.3) at time.3, use 2014 age multipliers
    transmission.rates.3 = make.transmission.array(parameters = parameters,
                                                   global.trate = sampled.parameters["trate.3"],
                                                   male.to.male.multiplier = sampled.parameters["male.to.male.multiplier"], 
                                                   female.to.male.multiplier = sampled.parameters["female.to.male.multiplier"],
                                                   female.age.multipliers =c(rep(0,length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 0, upper = 15))),
                                                                             rep(sampled.parameters["age.15.to.19.transmission.multiplier.3"],
                                                                                 length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 15, upper = 20))),
                                                                             rep(sampled.parameters["age.20.to.29.transmission.multiplier.3"],
                                                                                 length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 20, upper = 30))),
                                                                             rep(1,length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 30, upper = 40))),
                                                                             rep(sampled.parameters["age.40.to.49.transmission.multiplier.3"],
                                                                                 length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 40, upper = 50))),
                                                                             rep(sampled.parameters["age.50.and.over.transmission.multiplier.3"],
                                                                                 length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 50, upper = Inf))))*
                                                       age.sex.trans.multipliers$FEMALE.AGE.MULTIPLIERS.2014,
                                                   male.age.multipliers =c(rep(0,length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 0, upper = 15))),
                                                                           rep(sampled.parameters["age.15.to.19.transmission.multiplier.3"],
                                                                               length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 15, upper = 20))),
                                                                           rep(sampled.parameters["age.20.to.29.transmission.multiplier.3"],
                                                                               length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 20, upper = 30))),
                                                                           rep(1,length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 30, upper = 40))),
                                                                           rep(sampled.parameters["age.40.to.49.transmission.multiplier.3"],
                                                                               length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 40, upper = 50))),
                                                                           rep(sampled.parameters["age.50.and.over.transmission.multiplier.3"],
                                                                               length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 50, upper = Inf))))*
                                                       age.sex.trans.multipliers$MALE.AGE.MULTIPLIERS.2014)
    
    mixing.proportions.3 = mixing.proportions.0 # keep this the same for now
    transmission.rates.3=transmission.rates.3*mixing.proportions.3
    
    dim(transmission.rates.3) = c(n.trans.states, n.trans.states)
    transmission.rates.3 = as.matrix(transmission.rates.3)
    
    parameters = add.time.varying.parameter.value(parameters,
                                                  parameter.name='TRANSMISSION.RATES',
                                                  value = transmission.rates.3,
                                                  time = sampled.parameters['time.3'])
    
    # Set transmission rate to another level (trate.4) at time.3 (all multipliers, etc. the same as trate.3)
    transmission.rates.4 = make.transmission.array(parameters = parameters,
                                                   global.trate = sampled.parameters["trate.4"],
                                                   male.to.male.multiplier = sampled.parameters["male.to.male.multiplier"], 
                                                   female.to.male.multiplier = sampled.parameters["female.to.male.multiplier"],
                                                   female.age.multipliers =c(rep(0,length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 0, upper = 15))),
                                                                             rep(sampled.parameters["age.15.to.19.transmission.multiplier.3"],
                                                                                 length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 15, upper = 20))),
                                                                             rep(sampled.parameters["age.20.to.29.transmission.multiplier.3"],
                                                                                 length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 20, upper = 30))),
                                                                             rep(1,length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 30, upper = 40))),
                                                                             rep(sampled.parameters["age.40.to.49.transmission.multiplier.3"],
                                                                                 length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 40, upper = 50))),
                                                                             rep(sampled.parameters["age.50.and.over.transmission.multiplier.3"],
                                                                                 length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 50, upper = Inf))))*
                                                       age.sex.trans.multipliers$FEMALE.AGE.MULTIPLIERS.2014,
                                                   male.age.multipliers =c(rep(0,length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 0, upper = 15))),
                                                                           rep(sampled.parameters["age.15.to.19.transmission.multiplier.3"],
                                                                               length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 15, upper = 20))),
                                                                           rep(sampled.parameters["age.20.to.29.transmission.multiplier.3"],
                                                                               length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 20, upper = 30))),
                                                                           rep(1,length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 30, upper = 40))),
                                                                           rep(sampled.parameters["age.40.to.49.transmission.multiplier.3"],
                                                                               length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 40, upper = 50))),
                                                                           rep(sampled.parameters["age.50.and.over.transmission.multiplier.3"],
                                                                               length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 50, upper = Inf))))*
                                                       age.sex.trans.multipliers$MALE.AGE.MULTIPLIERS.2014)
    
    mixing.proportions.4 = mixing.proportions.0 # keep this the same for now
    transmission.rates.4=transmission.rates.4*mixing.proportions.4
    
    dim(transmission.rates.4) = c(n.trans.states, n.trans.states)
    transmission.rates.4 = as.matrix(transmission.rates.4)
    
    parameters = add.time.varying.parameter.value(parameters,
                                                  parameter.name='TRANSMISSION.RATES',
                                                  value = transmission.rates.4,
                                                  time = sampled.parameters['time.4'])
    
    
    transmission.rates.3.5 = transmission.rates.3 + (transmission.rates.4-transmission.rates.3)*sampled.parameters["proportion.trate.change.by.3.5"]
    parameters = add.time.varying.parameter.value(parameters,
                                                  parameter.name='TRANSMISSION.RATES',
                                                  value = transmission.rates.3.5,
                                                  time = sampled.parameters['time.3.5'])
    
    infectiousness.h = array(0,
                             dim=sapply(state.dim.names, length),
                             dimnames=state.dim.names)
    
    infectiousness.h[,,,'undiagnosed'] = 1
    infectiousness.h[,,,'diagnosed_unengaged'] = sampled.parameters['relative.transmission.from.diagnosis']
    infectiousness.h[,,,'engaged_unsuppressed'] = sampled.parameters['relative.transmission.from.diagnosis']
    
    parameters = add.time.varying.parameter.value(parameters,
                                                  parameter.name='INFECTIOUSNESS.H',
                                                  value = infectiousness.h,
                                                  time = 2000)
    
    # New HIV births
    maternal.fetal.transmission.0 = array(0,
                                        dim=sapply(state.dim.names, length),
                                        dimnames=state.dim.names)
    
    maternal.fetal.transmission.1 = maternal.fetal.transmission.0
    
    maternal.fetal.transmission.0[,,,c('undiagnosed','diagnosed_unengaged','engaged_unsuppressed')] = sampled.parameters['birth.transmission.risk.0']
    maternal.fetal.transmission.1[,,,c('undiagnosed','diagnosed_unengaged','engaged_unsuppressed')] = sampled.parameters['birth.transmission.risk.1']
    
    parameters = add.time.varying.parameter.value(parameters,
                                                  parameter.name='MATERNAL.FETAL.TRANSMISSION',
                                                  value = maternal.fetal.transmission.0,
                                                  time = sampled.parameters['birth.transmission.time.0'])
    
    parameters = add.time.varying.parameter.value(parameters,
                                                  parameter.name='MATERNAL.FETAL.TRANSMISSION',
                                                  value = maternal.fetal.transmission.1,
                                                  time = sampled.parameters['birth.transmission.time.1'])
    
    
    
    #-- ENGAGEMENT --#
    engagement.model = get.engagement.model(location = location)
    engagement.times = c(1975:min(project.to.year,sampled.parameters["cascade.improvement.end.year"], na.rm = T))
    
    # if(location=="Kenya"){
    #     engagement.rates = c(lapply(engagement.times, function(year){
    #         projected.log.odds = engagement.model$intercept+
    #             (engagement.model$slope+sampled.parameters['log.OR.engagement.slope'])*(year-engagement.model$anchor.year)
    #         
    #         projected.p = 1/(1+exp(-projected.log.odds)) 
    #         projected.p = projected.p*engagement.model$max.proportion 
    #         projected.rate = -log(1-projected.p)
    #         
    #         projected.rate.age.sex = array(projected.rate,
    #                                        dim=sapply(trans.dim.names, length),
    #                                        dimnames=trans.dim.names)
    #         projected.rate.age.sex[,"male",] = projected.rate.age.sex[,"male",]*sampled.parameters["male.engagement.multiplier"]
    #         
    #         projected.rate.age.sex
    #         
    #     }))
    # } else { 
    
    # using IeDEA dashboard method for all countries 
    engagement.rates = c(lapply(engagement.times, function(year){
        
        projected.log.odds = as.numeric(sapply(trans.dim.names$sex, function(sex){
            sapply(trans.dim.names$age, function(age){
                eng.cat = names(MODEL.TO.IEDEA.AGE.MAPPING)[sapply(MODEL.TO.IEDEA.AGE.MAPPING, function(x) age %in% x)]
                
                engagement.model$intercept + 
                    (engagement.model$slope+sampled.parameters['log.OR.engagement.slope'])*(year-engagement.model$anchor.year) + 
                    (engagement.model$age.10.19*(eng.cat=="10-19")) + 
                    (engagement.model$age.20.29*(eng.cat=="20-29")) + 
                    (engagement.model$age.40.49*(eng.cat=="40-49")) + 
                    (engagement.model$age.50.plus*(eng.cat=="50 and over")) + 
                    (engagement.model$sex.female*(sex=="female"))  
                
            })
        }))
        dim(projected.log.odds) = sapply(trans.dim.names,length)
        dimnames(projected.log.odds) = trans.dim.names
        
        projected.p = 1/(1+exp(-projected.log.odds)) 
        
        projected.p = projected.p*engagement.model$max.proportion 
        projected.rate = -log(1-projected.p)   
        projected.rate[,"male",] = projected.rate[,"male",]*sampled.parameters["male.engagement.multiplier"]
        projected.rate
        
    }))
    
    parameters = set.rates.for.interventions(baseline.rates = engagement.rates, # list 
                                             baseline.times = engagement.times, # vector
                                             interventions = interventions,
                                             scale = "rate", 
                                             parameters = parameters,
                                             parameter.name = "ENGAGEMENT.RATES")
    
    
    #-- DISENGAGEMENT --#
    unsuppressed.disengagement.times = c(2000)
    unsuppressed.disengagement.rates = list(array(sampled.parameters['unsuppressed.disengagement.rates'],
                                                  dim=sapply(trans.dim.names, length),
                                                  dimnames=trans.dim.names))
    
    parameters = set.rates.for.interventions(baseline.rates = unsuppressed.disengagement.rates, # list 
                                             baseline.times = unsuppressed.disengagement.times, # vector
                                             interventions = interventions,
                                             scale = "rate", 
                                             parameters = parameters,
                                             parameter.name = "UNSUPPRESSED.DISENGAGEMENT.RATES")
    
    suppressed.disengagement.times = c(2000)
    suppressed.disengagement.rates = list(array(sampled.parameters['suppressed.disengagement.rates'],
                                                  dim=sapply(trans.dim.names, length),
                                                  dimnames=trans.dim.names))
    
    parameters = set.rates.for.interventions(baseline.rates = suppressed.disengagement.rates, # list 
                                             baseline.times = suppressed.disengagement.times, # vector
                                             interventions = interventions,
                                             scale = "rate", 
                                             parameters = parameters,
                                             parameter.name = "SUPPRESSED.DISENGAGEMENT.RATES")
    
    
    
    #-- SUPPRESSION/UNSUPPRESSION --#
    suppression.model = get.suppression.rates(location = location)
    suppression.times = c(1975:min(project.to.year,sampled.parameters["cascade.improvement.end.year"], na.rm = T))
    
    suppression.rates = c(lapply(suppression.times, function(year){
        
        projected.log.odds = as.numeric(sapply(trans.dim.names$sex, function(sex){
            sapply(trans.dim.names$age, function(age){
                eng.cat = names(MODEL.TO.IEDEA.AGE.MAPPING)[sapply(MODEL.TO.IEDEA.AGE.MAPPING, function(x) age %in% x)]
                
                suppression.model$intercept + 
                    (suppression.model$slope+sampled.parameters['log.OR.suppression.slope'])*(year-suppression.model$anchor.year) + 
                    (suppression.model$age.10.19*(eng.cat=="10-19")) + 
                    (suppression.model$age.20.29*(eng.cat=="20-29")) + 
                    (suppression.model$age.40.49*(eng.cat=="40-49")) + 
                    (suppression.model$age.50.plus*(eng.cat=="50 and over")) + 
                    (suppression.model$sex.female*(sex=="female"))  
                
            })
        }))
        dim(projected.log.odds) = sapply(trans.dim.names,length)
        dimnames(projected.log.odds) = trans.dim.names
        
        projected.p = 1/(1+exp(-projected.log.odds)) 
        
        projected.p = projected.p*suppression.model$max.proportion 
        projected.rate = -log(1-projected.p)   
        projected.rate[,"male",] = projected.rate[,"male",]*sampled.parameters["male.suppression.multiplier"]
        projected.rate
        
    }))
    
    parameters = set.rates.for.interventions(baseline.rates = suppression.rates, # list 
                                             baseline.times = suppression.times, # vector
                                             interventions = interventions,
                                             scale = "rate", 
                                             parameters = parameters,
                                             parameter.name = "SUPPRESSION.RATES") 

    unsuppression.times = c(2000)
    unsuppression.rates = c(list(array(sampled.parameters['unsuppression.rates'],
                                       dim=sapply(trans.dim.names, length),
                                       dimnames=trans.dim.names)))
    
    parameters = set.rates.for.interventions(baseline.rates = unsuppression.rates, # list 
                                             baseline.times = unsuppression.times, # vector
                                             interventions = interventions,
                                             scale = "rate", 
                                             parameters = parameters,
                                             parameter.name = "UNSUPPRESSION.RATES")
    

       

    
    
    #-- RETURN --#
    
    parameters
}


#-- TIME VARYING PARAMETERS --#
# parameters$time.varying.parameters: list of time varying parameters

# Adds a time point and a value to the spline for a parameter; added to parameters$time.varying.parameters;
# cannot have multiple entries for the same time
add.time.varying.parameter.value <- function(parameters,
                                             parameter.name,
                                             time,
                                             value){
    # Get or make the entry
    if (is.null(parameters$time.varying.parameters[[parameter.name]]))
        param = list(
            times = numeric(),
            values = list()
        )
    else
        param = parameters$time.varying.parameters[[parameter.name]]
    
    # Check that the time is not already in there
    if (any(param$times==time))
        stop(paste0("The time ", time, " has already been entered as a spline point for parameter '", parameter.name, "'"))
    
    # Append the new values
    param$times = c(param$times, time)
    param$values = c(param$values, list(value)) 
    
    # Sort by time
    o = order(param$times)
    param$times = param$times[o]
    param$values = param$values[o]
    
    # Plug it in and return
    parameters$time.varying.parameters[[parameter.name]] = param
    parameters
}


# Computes the parameter value at a specific time; called at beginning of compute.dx function in diffeq code; 
# applied across all parameters$time.varying.parameters
compute.time.varying.parameters <- function(parameters, time){
    lapply(parameters$time.varying.parameters, function(params){
        # params is a list with two components
        # $times - a vector of times
        # $values - a list of values
        # assume these are ordered
        
        n.times = length(params$times)
        
        if (time <= params$times[1])
            params$values[[1]] #return first value
        else if (time >= params$times[n.times])
            params$values[[n.times]] #return last value
        else 
        {#we need to interpolate (linearly)
            index1 = (1:n.times)[params$times<=time] 
            index1 = index1[length(index1)] #find the index of last 'time' entry which is <= selected time
            index2 = index1 + 1 #the index of the first 'time' entry which is > selected time
            
            if (time==params$times[index1])
                params$values[[index1]] #return the value
            else #linear interpolation:
            {
                time1 = params$times[index1]
                time2 = params$times[index2]
                
                value1 = params$values[[index1]]
                value2 = params$values[[index2]]
                
                slope = (value2 - value1) / (time2 - time1)
                value1 + slope * (time - time1) #final value that is returned
            }
        }
        
    })
}



##---------------------------##
#-- OTHER/HELPER FUNCTIONS --#
##---------------------------##

# Multiplies global transmission rate by age and sex multipliers; called in map.model.parameters function 
# when setting up transmission rates; age and sex multipliers come from sampled.parameters 
make.transmission.array = function(parameters,
                                   global.trate,
                                   male.to.male.multiplier,
                                   female.to.male.multiplier,
                                   female.to.female.multiplier = 0,
                                   female.age.multipliers,
                                   male.age.multipliers){
    
    if(length(parameters$AGES)!=length(female.age.multipliers)) 
        stop("incorrect number of female age multipliers")
    
    if(length(parameters$AGES)!=length(male.age.multipliers)) 
        stop("incorrect number of male age multipliers")
    
    transmission.dim.names = list(age.to=parameters$AGES, 
                                  sex.to=parameters$SEXES,
                                  subgroup.to=parameters$SUBGROUPS,
                                  age.from=parameters$AGES, 
                                  sex.from=parameters$SEXES,
                                  subgroup.from=parameters$SUBGROUPS)
    
    rv = array(global.trate,
               sapply(transmission.dim.names, length),
               dimnames = transmission.dim.names)
    
    rv[,"female",,,,] = rv[,"female",,,,]*female.age.multipliers # transmission to females
    rv[,"male",,,,] = rv[,"male",,,,]*male.age.multipliers #transmission to males
    
    rv[,"male",,,"male",] = rv[,"male",,,"male",]*male.to.male.multiplier
    rv[,"male",,,"female",] = rv[,"male",,,"female",]*female.to.male.multiplier
    rv[,"female",,,"female",] = rv[,"female",,,"female",]*female.to.female.multiplier
    
    rv
}


# Helper function for creating interventions
set.rates.for.interventions = function(baseline.rates,
                                       baseline.times,
                                       interventions,
                                       scale,
                                       parameters,
                                       parameter.name){
    
    rates=baseline.rates
    times=baseline.times
    
    for(intervention in interventions){
        intervention.unit = intervention$units[[parameter.name]]
        
        ages = intervention$target.ages
        sexes = intervention$target.sexes
        if(is.null(ages)){
            ages = parameters$AGES}
        if(is.null(sexes)){
            sexes = parameters$SEXES}
        
        if(!is.null(intervention.unit)){
            
            new.times = c(times,intervention.unit$start.time,intervention.unit$effect.times,intervention.unit$end.time)
            new.times = sort(unique(new.times))
            new.times = new.times[new.times<Inf]

            new.rates = interpolate(values = rates, value.times = times, desired.times = new.times)
            names(new.rates) = new.times
            
            effect.values = convert.scales(values = intervention.unit$effect.values, 
                                           from.scale = intervention.unit$scale,
                                           to.scale = scale)
            
            for(i in 1:length(intervention.unit$effect.times)){
                time = as.character(intervention.unit$effect.times[i])
                
                if(!intervention.unit$allow.lower.than.baseline){
                    # only new values (effect values) that are greater than the baseline values (new.rates right now are baseline values)
                    mask = effect.values[i] > new.rates[[time]][ages,sexes,]
                    new.rates[[time]][ages,sexes,][mask] = effect.values[i]
                      
                } else if(!intervention.unit$allow.higher.than.baseline){
                    # only new values (effect values) that are lower than the baseline values (new.rates right now are baseline values)
                    mask = effect.values[i] < new.rates[[time]][ages,sexes,]
                    new.rates[[time]][ages,sexes,][mask] = effect.values[i]
                } else 
                    new.rates[[time]][ages,sexes,] = effect.values[i]
            }
            
            times.to.overwrite = new.times[new.times>intervention.unit$start.time & new.times<intervention.unit$end.time]
            interpolate.from.times = c(intervention.unit$start.time,intervention.unit$effect.times,intervention.unit$end.time)
            interpolate.from.times = interpolate.from.times[interpolate.from.times<Inf]
            
            for(time in times.to.overwrite){
                new.rates[[as.character(time)]][ages,sexes,] = interpolate(values = new.rates[as.character(interpolate.from.times)], 
                                                                           value.times = interpolate.from.times,
                                                                           desired.times = time)[[1]][ages,sexes,]
            }
            
            rates = new.rates
            times = new.times
            
        }
        
    }
    
    for(i in 1:length(rates)){
        parameters = add.time.varying.parameter.value(parameters,
                                                      parameter.name=parameter.name,
                                                      value = rates[[i]], 
                                                      time = times[i]) 
    }
    
    parameters
    
}


interpolate = function(values,
                       value.times,
                       desired.times){
    
    n.times = length(value.times)
    
    lapply(desired.times,function(time){
        
        if (time <= value.times[1])
            values[[1]] #return first value
        else if (time >= value.times[n.times])
            values[[n.times]] #return last value
        else 
        {#we need to interpolate (linearly)
            index1 = (1:n.times)[value.times<=time] 
            index1 = index1[length(index1)] #find the index of last 'time' entry which is <= selected time
            index2 = index1 + 1 #the index of the first 'time' entry which is > selected time
            
            if (time==value.times[index1] || value.times[index1]==-Inf)
                values[[index1]] #return the value
            else if(time==value.times[index2] || value.times[index2]==Inf) 
                values[[index2]]
            else #linear interpolation:
            {
                time1 = value.times[index1]
                time2 = value.times[index2]
                
                value1 = values[[index1]]
                value2 = values[[index2]]
                
                slope = (value2 - value1) / (time2 - time1)
                value1 + slope * (time - time1) #final value that is returned
            }
        }
        
    })
           
    

    
    
}





