
# Calculates death rates for correct model age stratifications, based on surveillance data
calculate.all.death.rates = function(data.manager,
                                     location,
                                     keep.dimensions = c('year','age','sex','location'),
                                     model.age.cutoffs){
    
    POPULATION.AGE.MAPPING = map.population.ages(data.manager = data.manager,
                                                 data.type = "population", 
                                                 model.age.cutoffs = model.age.cutoffs)
    
    years = data.manager$total.mortality$YEARS
    deaths.ages = data.manager$total.mortality$AGES
    #deaths.ages.rev = c(deaths.ages[-length(deaths.ages)],"95-99","100 and over")
    deaths.sexes = data.manager$total.mortality$SEXES
    
    age.dim.names = list(year = years,
                         age = deaths.ages) 
    
    age.sex.dim.names = list(year = years,
                             age = deaths.ages, 
                             sex = deaths.sexes)
    
    full.dim.names = list(year = years,
                          age = deaths.ages, 
                          sex = deaths.sexes,
                          location=location)
    
    ## Pull deaths
    deaths = get.surveillance.data(data.manager = data.manager,
                                   location = location,
                                   data.type = "total.mortality",
                                   years = years,
                                   keep.dimensions = keep.dimensions)
    ## Pull population
    pop = get.surveillance.data(data.manager = data.manager,
                                location = location,
                                data.type = "population",
                                years = years,
                                keep.dimensions = keep.dimensions) 
    
    ## Dim names
    if(length(keep.dimensions)==1){
        death.rate.dim.names = list(year = years)
    } else if (setequal(keep.dimensions, c('year','age'))){
        death.rate.dim.names = age.dim.names
    } else if (setequal(keep.dimensions, c('year','sex'))){
        death.rate.dim.names = list(year = years,
                                    sex = deaths.sexes)
    } else if (setequal(keep.dimensions, c('year','age','sex'))){
        death.rate.dim.names = age.sex.dim.names
    } else if (setequal(keep.dimensions, c('year','age','sex'))){
        death.rate.dim.names = age.sex.dim.names
    } else if(setequal(keep.dimensions, c('year','age','sex','location'))){
        death.rate.dim.names = full.dim.names
    } else
        stop("Incorrect keep.dimensions in calculate.all.death.rates")
    
    ## Divide deaths by population, mapping to model age brackets 
    if(length(keep.dimensions)==1){
        rv = sapply(years, function(year){
            (deaths[year])/(pop[year])
        })
        
        new.dim.names = list(year = years)
    } else if (setequal(keep.dimensions, c('year','age'))){
        rv = sapply(1:length(POPULATION.AGE.MAPPING), function(age){
            sapply(1:length(years), function(year){
                age.to = names(POPULATION.AGE.MAPPING)[age] # names of mapping are the model ages - what I want to map TO
                ages.from = POPULATION.AGE.MAPPING[[age]] # list elements are the population ages - what I want to map FROM
                sum(deaths[year,ages.from])/sum(pop[year,ages.from])
            })
        })
        
        new.dim.names = list(year = years,
                             age = names(POPULATION.AGE.MAPPING))
    } else if (setequal(keep.dimensions, c('year','sex')) ){
        rv = sapply(deaths.sexes, function(sex){
            sapply(1:length(years), function(year){
                (deaths[year,sex])/(pop[year,sex])
            })
        })
        
        new.dim.names = list(year = years,
                             sex = deaths.sexes)
    } else if (setequal(keep.dimensions, c('year','age','sex'))){
        rv = sapply(deaths.sexes, function(sex){
            sapply(1:length(POPULATION.AGE.MAPPING), function(age){
                sapply(1:length(years), function(year){
                    age.to = names(POPULATION.AGE.MAPPING)[age] # names of mapping are the model ages - what I want to map TO
                    ages.from = POPULATION.AGE.MAPPING[[age]] # list elements are the population ages - what I want to map FROM
                    sum(deaths[year,ages.from,sex])/sum(pop[year,ages.from,sex])
                })
            })
        })
        
        new.dim.names = list(year = years,
                             age = names(POPULATION.AGE.MAPPING),
                             sex = deaths.sexes)
    } else if (setequal(keep.dimensions, c('year','age','sex','location'))){
        rv = sapply(location, function(location){ 
            sapply(deaths.sexes, function(sex){
                sapply(1:length(POPULATION.AGE.MAPPING), function(age){
                    sapply(1:length(years), function(year){
                        age.to = names(POPULATION.AGE.MAPPING)[age] # names of mapping are the model ages - what I want to map TO
                        ages.from = POPULATION.AGE.MAPPING[[age]] # list elements are the population ages - what I want to map FROM
                        sum(deaths[year,ages.from,sex,location])/sum(pop[year,ages.from,sex,location])
                    })
                })
            })
        })
        new.dim.names = list(year = years,
                             age = names(POPULATION.AGE.MAPPING),
                             sex = deaths.sexes,
                             location = location)
    } else
        stop("Incorrect keep.dimensions in calculate.all.death.rates")
    
    dim(rv) = sapply(new.dim.names, length)
    dimnames(rv) = new.dim.names
    
    rv
    
}

# Projects future death rates based on logistic regression by age and sex 
project.deaths = function(data.manager,
                          location,
                          project.to.year,
                          keep.dimensions = c('year','sex','age','location'),
                          model.age.cutoffs,
                          sampled.parameters){
    
    deaths.age.sex.location = calculate.all.death.rates(data.manager = data.manager, 
                                                        location = location,
                                                        keep.dimensions = keep.dimensions, 
                                                        model.age.cutoffs = model.age.cutoffs)
    
    deaths.age.sex = deaths.age.sex.location[,,,location]
    
    #setting up code to smooth/project death rate into future 
    anchor.year = 1980
    years = as.numeric(dimnames(deaths.age.sex)$year) - anchor.year
    years.label = as.numeric(dimnames(deaths.age.sex)$year) # for plotting
    
    project.years = (max(years)+1):(project.to.year-anchor.year)-max(years)
    project.years = project.years[project.years%%5==0] # only including multiples of 5
    desired.years = c(years,max(years)+project.years) # future years to predict
    smoothed.years.label = desired.years + anchor.year # for plotting
    mask = rep(T,length(years)) # use this to remove years
    
    #mask = years.label<1987 # pre-HIV only 
    mask = (years.label<1987 | (years.label > 2007 & years.label < 2020)) # pre- and post-HIV; no COVID 
    
    mortality.intercepts.slopes.age.sex = apply(deaths.age.sex,c('age','sex'),function(rates){
        fit = lm(log(rates[mask]) ~ years[mask])
        rv = fit$coefficients
        names(rv) = c("intercept","slope")
        rv
    })
    
    age.45.to.65.age.brackets = get.age.brackets.in.range(lower = 45, 
                                                          upper = 65) 
    over.65.age.brackets = get.age.brackets.in.range(lower = 65, 
                                                     upper = Inf) 
    
    ## Add log multipliers, male
    mortality.intercepts.slopes.age.sex["intercept",age.45.to.65.age.brackets,"male"] = 
        mortality.intercepts.slopes.age.sex["intercept",age.45.to.65.age.brackets,"male"] +
        log(sampled.parameters['age.45.to.65.mortality.intercept.multiplier.male'])
    
    mortality.intercepts.slopes.age.sex["slope",age.45.to.65.age.brackets,"male"] = 
        mortality.intercepts.slopes.age.sex["slope",age.45.to.65.age.brackets,"male"] +
        log(sampled.parameters['age.45.to.65.mortality.slope.multiplier.male'])
    
    mortality.intercepts.slopes.age.sex["intercept",over.65.age.brackets,"male"] = 
        mortality.intercepts.slopes.age.sex["intercept",over.65.age.brackets,"male"] +
        log(sampled.parameters['over.65.mortality.intercept.multiplier.male'])
    
    mortality.intercepts.slopes.age.sex["slope",over.65.age.brackets,"male"] = 
        mortality.intercepts.slopes.age.sex["slope",over.65.age.brackets,"male"] +
        log(sampled.parameters['over.65.mortality.slope.multiplier.male'])
    
    ## Add log multipliers, female
    mortality.intercepts.slopes.age.sex["intercept",age.45.to.65.age.brackets,"female"] = 
        mortality.intercepts.slopes.age.sex["intercept",age.45.to.65.age.brackets,"female"] +
        log(sampled.parameters['age.45.to.65.mortality.intercept.multiplier.female'])

    mortality.intercepts.slopes.age.sex["slope",age.45.to.65.age.brackets,"female"] = 
        mortality.intercepts.slopes.age.sex["slope",age.45.to.65.age.brackets,"female"] +
        log(sampled.parameters['age.45.to.65.mortality.slope.multiplier.female'])

    mortality.intercepts.slopes.age.sex["intercept",over.65.age.brackets,"female"] = 
        mortality.intercepts.slopes.age.sex["intercept",over.65.age.brackets,"female"] +
        log(sampled.parameters['over.65.mortality.intercept.multiplier.female'])
    
    mortality.intercepts.slopes.age.sex["slope",over.65.age.brackets,"female"] = 
        mortality.intercepts.slopes.age.sex["slope",over.65.age.brackets,"female"] +
        log(sampled.parameters['over.65.mortality.slope.multiplier.female'])
    
    # Smoothed non-HIV mortality: fit regression on desired years only (using mask above)
    smooth.deaths.age.sex = apply(mortality.intercepts.slopes.age.sex,c('age','sex'),function(intercept.slope){
        exp(intercept.slope[1] + intercept.slope[2]*desired.years) #gives projections; exponentiate if log
    })
    dim.names = list(year = smoothed.years.label,
                     age = dimnames(smooth.deaths.age.sex)[2]$age,
                     sex = dimnames(smooth.deaths.age.sex)[3]$sex)
    dim(smooth.deaths.age.sex) = sapply(dim.names, length)
    dimnames(smooth.deaths.age.sex) = dim.names
    
    smooth.deaths.age.sex
}


## PLOTTING SUMMARIES OF PROJECTIONS 
if(1==2){
    LOCATION = "South Africa"
    params = get.default.parameters(location = LOCATION)
    #params["age.45.to.65.mortality.slope.multiplier"] = 1.02 # this helps hit male 45-65 in Kenya but messes up female 
    
    smoothed.deaths = project.deaths(data.manager = DATA.MANAGER,
                       location = LOCATION,
                       project.to.year = 2040,
                       model.age.cutoffs = MODEL.AGE.CUTOFFS,
                       sampled.parameters = params) #params.last
    
    smoothed.deaths.total = apply(smoothed.deaths,"year",median) # mean or median? 
    
    dim(smoothed.deaths.total) = dim(smoothed.deaths)[1]
    dimnames(smoothed.deaths.total) =  list(year = dimnames(smoothed.deaths)$year)
    
    df.smoothed.deaths = melt(smoothed.deaths)
    df.smoothed.deaths.total = melt(smoothed.deaths.total)
    
    # ggplot(df.smoothed.deaths[df.smoothed.deaths$sex == "male", ], aes(x = year, y = value*1000)) +
    #     geom_line() + facet_wrap(~ age, scales = "free_y")
    
    # ggplot(df.smoothed.deaths.total, aes(x = year, y = value*1000)) +
    #     geom_line() #+ geom_hline(yintercept = 8) + geom_vline(xintercept = 1993)
    
    crude.deaths = calculate.all.death.rates(data.manager = DATA.MANAGER,
                                  location = LOCATION,
                                  model.age.cutoffs = MODEL.AGE.CUTOFFS) 
    
    crude.deaths.total = apply(crude.deaths,"year",median) # mean or median? 
    
    dim(crude.deaths.total) = dim(crude.deaths)[1]
    dimnames(crude.deaths.total) =  list(year = dimnames(crude.deaths)$year)
    
    df.crude.deaths = melt(crude.deaths)
    df.crude.deaths.total = melt(crude.deaths.total)

    # ggplot() +
    #     geom_line(data = df.crude.deaths.total, aes(x = year, y = value*1000)) +  #+ geom_hline(yintercept = 13) + geom_vline(xintercept = 2008)
    #     geom_line(data = df.smoothed.deaths.total, aes(x = year, y = value*1000))+ 
    #     ggtitle(LOCATION)
    
    ggplot() +
        geom_line(data = df.crude.deaths[df.crude.deaths$sex == "male", ], aes(x = year, y = value*1000)) + 
        geom_line(data = df.smoothed.deaths[df.smoothed.deaths$sex == "male", ], aes(x = year, y = value*1000)) + 
        facet_wrap(~ age, scales = "free_y") + 
        ggtitle(paste0(LOCATION,", male"))
    
    ggplot() +
        geom_line(data = df.crude.deaths[df.crude.deaths$sex == "female", ], aes(x = year, y = value*1000)) + 
        geom_line(data = df.smoothed.deaths[df.smoothed.deaths$sex == "female", ], aes(x = year, y = value*1000)) + 
        facet_wrap(~ age, scales = "free_y") + 
        ggtitle(paste0(LOCATION,", female"))
    
    
}