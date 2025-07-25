########################################################
# Description: Functions to create the likelihood object
########################################################

# Functions 
#     1. create.likelihood 
#     2. get.likelihood.elements.by.data.type
#     3. get.likelihood.elements.by.data.type.and.dimension 
#     4. compute.likelihood
#     5. create.likelihood.for.data.type



# incidence, prevalence, engagement, suppression, population, hiv mortality, awareness


library("mvtnorm")

WEIGHT.YEARS = 1970:2030
WEIGHTS.BY.YEAR = rep(1, length(WEIGHT.YEARS))
WEIGHTS.BY.YEAR = (1/4)^(WEIGHT.YEARS<2000) # before 2000, 1/4x
WEIGHTS.BY.YEAR[WEIGHT.YEARS>=2018] = 4 # from 2018, 4x
names(WEIGHTS.BY.YEAR) = WEIGHT.YEARS
WEIGHTS.BY.YEAR = WEIGHTS.BY.YEAR/8

# for france only, remove pre-1995 years 
WEIGHTS.BY.YEAR.FRANCE = WEIGHTS.BY.YEAR
WEIGHTS.BY.YEAR.FRANCE = WEIGHTS.BY.YEAR.FRANCE[as.character(1995:2030)]
WEIGHTS.BY.YEAR.FRANCE = WEIGHTS.BY.YEAR.FRANCE/8


# WEIGHTS.BY.YEAR = list(WEIGHTS.BY.YEAR)
# WEIGHTS.BY.YEAR$kenya = WEIGHTS.BY.YEAR
# WEIGHTS.BY.YEAR$south_africa = WEIGHTS.BY.YEAR
# WEIGHTS.BY.YEAR$france = WEIGHTS.BY.YEAR.FRANCE

# Calls individual "create.likelihood.for.data.type" functions for each data type
# Each data type function assembles the likelihood elements once (because it is time consuming - e.g., matrix M), 
# and then returns a function that computes the likelihood 
# SO, the output that create.likelihood() generates is a FUNCTION that can then be run on a sim to return the likelihood 
create.likelihood = function(data.manager=DATA.MANAGER,
                             parameters,
                             location,
                             years = 1995:2023, # 1980:2023
                             total.weight = WEIGHTS.BY.YEAR, # [[convert_string(location)]], 
                             #incidence
                             incidence.years=years,
                             incidence.weight=2, 
                             incidence.obs.correlation=0.5,
                             incidence.correlation.structure="auto.regressive",
                             #prevalence
                             prevalence.years=years,
                             prevalence.weight=1, 
                             prevalence.obs.correlation=0.5,
                             prevalence.correlation.structure="auto.regressive",
                             #awareness
                             awareness.years=years,
                             awareness.weight=1*3.734568, # ratio of points, when accounting for weighting by year (see data_point_weighting)
                             awareness.obs.correlation=0.5,
                             awareness.correlation.structure="compound.symmetry",
                             #engagement
                             engagement.years=years,
                             engagement.weight=1*3.734568, # ratio of points, when accounting for weighting by year (see data_point_weighting)
                             engagement.obs.correlation=0.5,
                             engagement.correlation.structure="compound.symmetry",
                             #suppression
                             suppression.years=years,
                             suppression.weight=1*3.734568, # ratio of points, when accounting for weighting by year (see data_point_weighting)
                             suppression.obs.correlation=0.5,
                             suppression.correlation.structure="compound.symmetry",
                             #population
                             population.years=years,
                             population.weight=1/200000, # have to downweight a lot due to large pop size/number of strata
                             population.obs.correlation=0.5,
                             population.correlation.structure="auto.regressive",
                             #hiv.mortality
                             hiv.mortality.years=years,
                             hiv.mortality.weight=1/256, 
                             hiv.mortality.obs.correlation=0.5, 
                             hiv.mortality.correlation.structure="auto.regressive",
                             #total.mortality
                             total.mortality.years=years,
                             total.mortality.weight=1/1000, 
                             total.mortality.weight.by.age = c("80 and over" = 0.25), # changed from 0.25
                             total.mortality.obs.correlation=0.5, 
                             total.mortality.correlation.structure="auto.regressive"
                             ){ 
    
    incidence.lik = create.likelihood.for.data.type(data.type = "incidence",
                                                    data.manager=data.manager,
                                                    years=incidence.years,
                                                    location=location,
                                                    parameters=parameters,
                                                    denominator.data.type="population", 
                                                    obs.is.proportion=F,
                                                    weight=total.weight*incidence.weight,
                                                    obs.correlation=incidence.obs.correlation,
                                                    correlation.structure=incidence.correlation.structure #,
                                                    # use.total=T, # defaults for all of these are TRUE
                                                    # use.sex=F,
                                                    # use.age=F,
                                                    # use.age.sex=F
                                                    )
    
    prevalence.lik = create.likelihood.for.data.type(data.type = "prevalence",
                                                     data.manager=data.manager,
                                                     years=prevalence.years,
                                                     location=location,
                                                     parameters=parameters,
                                                     denominator.data.type="population", 
                                                     obs.is.proportion=F,
                                                     weight=total.weight*prevalence.weight,
                                                     obs.correlation=prevalence.obs.correlation,
                                                     correlation.structure=prevalence.correlation.structure #,
                                                     # use.total=T, # defaults for all of these are TRUE
                                                     # use.sex=F,
                                                     # use.age=F,
                                                     # use.age.sex=F
                                                     )
    
    awareness.lik = create.likelihood.for.data.type(data.type = "awareness",
                                                    data.manager=data.manager,
                                                    years=awareness.years,
                                                    location=location,
                                                    parameters=parameters,
                                                    denominator.data.type="prevalence",
                                                    obs.is.proportion=T, # awareness data is reported as a proportion 
                                                    weight=total.weight*awareness.weight,
                                                    obs.correlation=awareness.obs.correlation,
                                                    correlation.structure=awareness.correlation.structure)
    
    engagement.lik = create.likelihood.for.data.type(data.type = "engagement",
                                                     data.manager=data.manager,
                                                     years=engagement.years,
                                                     location=location,
                                                     parameters=parameters,
                                                     denominator.data.type="awareness", # engagement denominator = awareness
                                                     obs.is.proportion=T, # engagement data is reported as a proportion 
                                                     weight=total.weight*engagement.weight,
                                                     obs.correlation=engagement.obs.correlation,
                                                     correlation.structure=engagement.correlation.structure)
    
    suppression.lik = create.likelihood.for.data.type(data.type = "suppression",
                                                      data.manager=data.manager,
                                                      years=suppression.years,
                                                      location=location,
                                                      parameters=parameters,
                                                      denominator.data.type="awareness", # suppression denominator = awareness
                                                      obs.is.proportion=T, # suppression data is reported as a proportion 
                                                      weight=total.weight*suppression.weight,
                                                      obs.correlation=suppression.obs.correlation,
                                                      correlation.structure=suppression.correlation.structure)
    
    population.lik = create.likelihood.for.data.type(data.type = "population",
                                                     data.manager=data.manager,
                                                     years=population.years,
                                                     location=location,
                                                     parameters=parameters,
                                                     denominator.data.type=NULL, 
                                                     obs.is.proportion=F,
                                                     weight=total.weight*population.weight,
                                                     obs.correlation=population.obs.correlation,
                                                     correlation.structure=population.correlation.structure,
                                                     calculate.sds.from.ci=F,
                                                     use.total=F,
                                                     use.sex=F,
                                                     use.age=F,
                                                     use.age.sex=T) 
    
    hiv.mortality.lik = create.likelihood.for.data.type(data.type = "hiv.mortality",
                                                        divide.obs.by.denominator=T,
                                                        data.manager=data.manager,
                                                        years=hiv.mortality.years,
                                                        location=location,
                                                        parameters=parameters,
                                                        denominator.data.type="prevalence", # technically hiv mortality reported as a number
                                                        obs.is.proportion=T, 
                                                        weight=total.weight*hiv.mortality.weight,
                                                        obs.correlation=hiv.mortality.obs.correlation,
                                                        correlation.structure=hiv.mortality.correlation.structure)
    
    total.mortality.lik = create.likelihood.for.data.type(data.type = "total.mortality",
                                                          data.manager=data.manager,
                                                          years=total.mortality.years,
                                                          location=location,
                                                          parameters=parameters,
                                                          denominator.data.type=NULL, 
                                                          obs.is.proportion=F,
                                                          weight=total.mortality.weight, # *total.weight # removed 1/8
                                                          weight.by.age=total.mortality.weight.by.age,
                                                          obs.correlation=total.mortality.obs.correlation,
                                                          correlation.structure=total.mortality.correlation.structure,
                                                          calculate.sds.from.ci=F,
                                                          use.total=F,
                                                          use.sex=F,
                                                          use.age=F,
                                                          use.age.sex=T) 
    
    awareness.trend.lik = create.likelihood.for.trend(data.type = "awareness",
                                                      year.1=2025,
                                                      year.2=2030,
                                                      probability.of.decrease=.1,
                                                      use.strata=F)
    
    # awareness.stratum.trend.lik = create.likelihood.for.trend(data.type = "awareness",
    #                                                   year.1=2025,
    #                                                   year.2=2030,
    #                                                   probability.of.decrease=.3,
    #                                                   use.strata=T)
    
    components = list(incidence=incidence.lik,
                      prevalence=prevalence.lik,
                      awareness=awareness.lik,
                      engagement=engagement.lik,
                      suppression=suppression.lik,
                      population=population.lik,
                      hiv.mortality=hiv.mortality.lik,
                      total.mortality.lik=total.mortality.lik,
                      awareness.trend=awareness.trend.lik
                      ) # CHANGE THIS IF SWITCHING ABOVE 
    
    rv = function(sim){ 
        
        # checking if any components of the sim are NA
        if(any(sapply(sim,function(x){
            any(is.na(x))
        }))){
            rv = -Inf
           # stop("NA values in sim")
        } else{
            rv = sum(sapply(components, function(likelihood){likelihood(sim)})) # adding up each likelihood component, run on sim    
        }
        
        return(rv)
    }
    
    attr(rv,"components") = components # attaching the components as an attribute to the function 
    
    # returns a function that can then be run on a sim to return the likelihood 
    rv
}

create.likelihood.for.trend = function(data.type,
                                       year.1,
                                       year.2,
                                       probability.of.decrease,
                                       use.strata){
    
    function(sim){
        compute.trend.likelihood(sim=sim,
                                 data.type=data.type,
                                 year.1=year.1,
                                 year.2=year.2,
                                 p=probability.of.decrease,
                                 use.strata=use.strata)
    }
    
}

compute.trend.likelihood = function(sim,
                                    data.type,
                                    year.1,
                                    year.2,
                                    use.strata,
                                    p){

    if(use.strata)
        keep.dimensions = c("year","age","sex")
    else
        keep.dimensions="year"
    
    value.1 = extract.data(sim=sim,
                           data.type=data.type,
                           keep.dimensions = keep.dimensions,
                           years=year.1)

    value.2 = extract.data(sim=sim,
                           data.type=data.type,
                           keep.dimensions = keep.dimensions,
                           years=year.2)
    
    if(data.type=="awareness"){
        denominator.1 = extract.data(sim=sim,
                                     data.type="prevalence",
                                     keep.dimensions = keep.dimensions,
                                     years=year.1)
        value.1 = value.1/denominator.1
        
        denominator.2 = extract.data(sim=sim,
                                     data.type="prevalence",
                                     keep.dimensions = keep.dimensions,
                                     years=year.2)
        value.2 = value.2/denominator.2
    }
    
    y = as.numeric(value.2<value.1) # decreasing
    
    # if y=1 (decreasing), d=p
    # if y=0 (increasing or flat), d=(1-p)
    # d = (p^y)*((1-p)^(1-y))
    
    # log version - not sure which to use/how
    d = y*log(p) + (1-y)*log(1-p)
    
    # return the density 
    rv = sum(d) # log likelihood so can sum these
    
}

# Using a call to get.likelihood.elements.by.data.type, assembles the likelihood elements once per data type (slow), 
# then returns a FUNCTION that calls compute.likelihood, computes the likelihood on those elements (faster)
create.likelihood.for.data.type = function(data.type,
                                           divide.obs.by.denominator=F,
                                           data.manager,
                                           years,
                                           location,
                                           denominator.data.type,
                                           obs.is.proportion,
                                           weight,
                                           weight.by.age=NULL,
                                           obs.correlation,
                                           correlation.structure,
                                           parameters,
                                           calculate.sds.from.ci=T,
                                           use.total=T,
                                           use.sex=T,
                                           use.age=T,
                                           use.age.sex=T){

    # slower; so only have to call this function once and then creates the below function
    likelihood.elements = get.likelihood.elements.by.data.type(data.type=data.type,
                                                               divide.obs.by.denominator=divide.obs.by.denominator,
                                                               denominator.data.type=denominator.data.type,
                                                               years=years,
                                                               location=location,
                                                               data.manager=data.manager,
                                                               parameters=parameters,
                                                               obs.correlation=obs.correlation,
                                                               correlation.structure=correlation.structure,
                                                               calculate.sds.from.ci=calculate.sds.from.ci,
                                                               use.total=use.total,
                                                               use.sex=use.sex,
                                                               use.age=use.age,
                                                               use.age.sex=use.age.sex
                                                               )
    
    function(sim,debug=F){ # this gets called every single time we invoke likelihood on a simulation; which is why we pre-compute above it
        
        compute.likelihood(sim=sim,
                           numerator.data.type=data.type,
                           denominator.data.type=denominator.data.type,
                           M=likelihood.elements$M, 
                           obs.is.proportion=obs.is.proportion,
                           obs.year=likelihood.elements$obs.year,
                           obs.dimensions=likelihood.elements$obs.dimensions,
                           years=years,
                           obs=likelihood.elements$obs, 
                           obs.cov.mat=likelihood.elements$obs.cov.mat,
                           weight=weight,
                           weight.by.age=weight.by.age,
                           debug=debug)
        
    }
    
}



## Calls get.likelihood.elements.by.data.type.and.dimension once for each combo of dimensions (year, year/age, etc.)
## Will be called above (in create.likelihood.for.data.type) once per data type 
get.likelihood.elements.by.data.type = function(data.type,
                                                divide.obs.by.denominator=divide.obs.by.denominator,
                                                denominator.data.type=denominator.data.type,
                                                years,
                                                location,
                                                data.manager,
                                                obs.correlation,
                                                correlation.structure,
                                                parameters,
                                                calculate.sds.from.ci=T,
                                                use.total=T,
                                                use.sex=T,
                                                use.age=T,
                                                use.age.sex=T){
    if(use.total)
        dim.1 = get.likelihood.elements.by.data.type.and.dimension(data.type = data.type,
                                                                   divide.obs.by.denominator=divide.obs.by.denominator,
                                                                   denominator.data.type=denominator.data.type,
                                                                   years = years,
                                                                   location = location,
                                                                   data.manager = data.manager,
                                                                   parameters=parameters,
                                                                   calculate.sds.from.ci=calculate.sds.from.ci,
                                                                   keep.dimensions = c("year","location"))
    else
        dim.1 = NULL
    
    if(use.age)
        dim.2 = get.likelihood.elements.by.data.type.and.dimension(data.type = data.type,
                                                                   divide.obs.by.denominator=divide.obs.by.denominator,
                                                                   denominator.data.type=denominator.data.type,
                                                                   years = years,
                                                                   location = location,
                                                                   data.manager = data.manager,
                                                                   parameters=parameters,
                                                                   calculate.sds.from.ci=calculate.sds.from.ci,
                                                                   keep.dimensions = c("year","age","location"))
    else 
        dim.2 = NULL
    
    if(use.sex)
        dim.3 = get.likelihood.elements.by.data.type.and.dimension(data.type = data.type,
                                                                   divide.obs.by.denominator=divide.obs.by.denominator,
                                                                   denominator.data.type=denominator.data.type,
                                                                   years = years,
                                                                   location = location,
                                                                   data.manager = data.manager,
                                                                   parameters=parameters,
                                                                   calculate.sds.from.ci=calculate.sds.from.ci,
                                                                   keep.dimensions = c("year","sex","location"))
    else
        dim.3 = NULL
    
    if(use.age.sex)
        dim.4 = get.likelihood.elements.by.data.type.and.dimension(data.type = data.type,
                                                                   divide.obs.by.denominator=divide.obs.by.denominator,
                                                                   denominator.data.type=denominator.data.type,
                                                                   years = years,
                                                                   location = location,
                                                                   data.manager = data.manager,
                                                                   parameters=parameters,
                                                                   calculate.sds.from.ci=calculate.sds.from.ci,
                                                                   keep.dimensions = c("year","age","sex","location"))
    else
        dim.4 = NULL

    # rename rv's to dim.1 
    rv = list()
    rv$M = rbind(dim.1$M, dim.2$M, dim.3$M, dim.4$M)
    rv$obs = c(dim.1$obs, dim.2$obs, dim.3$obs, dim.4$obs) 
    rv$obs.sds = c(dim.1$obs.sds, dim.2$obs.sds, dim.3$obs.sds,dim.4$obs.sds)
    rv$obs.year = c(dim.1$obs.year,dim.2$obs.year, dim.3$obs.year,dim.4$obs.year)
    rv$obs.dimensions = c(dim.1$obs.dimensions,dim.2$obs.dimensions, dim.3$obs.dimensions,dim.4$obs.dimensions)
    
    # Compound symmetry good for shorter periods (correlations between 1 year apart same as 10 years apart)
    if(correlation.structure=="compound.symmetry"){
        obs.corr.mat = sapply(1:length(rv$obs),function(i){
            sapply(1:length(rv$obs),function(j){
                
                # if the same observation (same year and same strata type) --> 1
                if(i==j) 
                    1
                # if the dimensions (e.g., year/sex) & strata type (e.g., male) of the observations are the same, but they are different years
                else if(rv$obs.dimensions[i]==rv$obs.dimensions[j]) 
                    obs.correlation 
                # if the observations are from different strata --> 0 
                else 
                    0
            })
        }) 
    }

    # Auto-regressive good for longer periods (correlations between 1 year apart stronger than 10 years apart)
    else if(correlation.structure=="auto.regressive") {
        diff = matrix(abs(rep(rv$obs.year,length(rv$obs.year))-rep(rv$obs.year,each=length(rv$obs.year))),
                      nrow=length(rv$obs.year))
        obs.corr.mat = obs.correlation^diff
        
        # correlation only applies if they're the same stratification of observation; otherwise 0 correlation
        obs.corr.mat = obs.corr.mat*as.numeric((rep(rv$obs.dimensions,length(rv$obs.dimensions))==rep(rv$obs.dimensions,each=length(rv$obs.dimensions))))
    }
    
    else 
        stop("incorrect correlation structure")
    
    # SDs * correlation matrix
    rv$obs.cov.mat = (rv$obs.sds %*% t(rv$obs.sds)) * obs.corr.mat

    rv
}

# Lowest-level function 
# Assembles observed data, maps each observation to the stratifications present in the model, 
# Returns a list with: 
        # (1) M matrix mapping observations to model stratifications 
        # (2) vector of observations 
        # (3) vector of observation years
        # (4) dimensions of each observation (e.g., age, age/sex)
        # (5) vector of observation standard deviations 
# This will be called once for each dimension (year, year/age, year/sex, year/age/sex)
get.likelihood.elements.by.data.type.and.dimension = function(data.type,
                                                              divide.obs.by.denominator=F,
                                                              denominator.data.type,
                                                              years,
                                                              location,
                                                              data.manager,
                                                              keep.dimensions,
                                                              parameters,
                                                              calculate.sds.from.ci=T){
    obs.data = get.surveillance.data(data.manager = data.manager,
                                     data.type = data.type,
                                     years = years,
                                     locations = location,
                                     keep.dimensions = keep.dimensions)
    if(divide.obs.by.denominator){
        denominator = get.surveillance.data(data.manager = data.manager,
                                            data.type = denominator.data.type,
                                            years = years,
                                            locations = location,
                                            keep.dimensions = keep.dimensions)
        
        obs.data = obs.data/denominator
    }
    
    obs.data.long = melt(obs.data)
    
    # For data with upper/lower values (everything except pop), use these to calculate the SDs; only keep observations with all three values 
    if(calculate.sds.from.ci){
        obs.data.uppers = as.numeric(get.surveillance.data(data.manager = data.manager,
                                                           data.type = paste0(data.type,".lowers"),
                                                           years = years,
                                                           locations = location,
                                                           keep.dimensions = keep.dimensions))
        obs.data.lowers = as.numeric(get.surveillance.data(data.manager = data.manager,  # placeholder 
                                                           data.type = paste0(data.type,".uppers"),
                                                           years = years,
                                                           locations = location,
                                                           keep.dimensions = keep.dimensions))
        
        if(divide.obs.by.denominator){
            obs.data.uppers = obs.data.uppers/denominator
            obs.data.lowers = obs.data.lowers/denominator
        }
        
        remove.mask = is.na(obs.data.long$value) | is.na(obs.data.uppers) | is.na(obs.data.lowers)
        obs.data.long = obs.data.long[!remove.mask,] # drop missing observations
        obs.data.uppers = obs.data.uppers[!remove.mask] 
        obs.data.lowers = obs.data.lowers[!remove.mask] 
    }

    else{
        obs.data.long = obs.data.long[!is.na(obs.data.long$value),]
    }

    
    if(nrow(obs.data.long)!=0){ ## if this dimension doesn't exist (e.g., no year/sex for incidence); returns a null list later
        
        dim.names = list(year = years,
                         age = parameters$AGES,
                         sex = parameters$SEXES)
        
        ## Fill in M matrix that maps observed data to stratifications present in model 
        ## Each row of M is a single observation, with 1's for each year/model age group/sex represented 
        M = NULL
        for(i in 1:nrow(obs.data.long)){
            years.to.pull = as.character(obs.data.long[i,"year"])
            ages.to.pull = obs.data.long[i,"age"]
            
            if(all(names(obs.data.long)!="age")){ # if no age strata, assign all ages
                ages.to.pull = "All ages"}
            
            if(!(data.type %in% c("population","total.mortality")))
                ages.to.pull = MODEL.TO.SURVEILLANCE.AGE.MAPPING[[as.character(ages.to.pull)]] # MELISSA CHECK IF ALL OTHER DATA TYPES USE THIS
            
            if(all(names(obs.data.long)!="sex")){ # if no sex strata, assign both sexes
                sexes.to.pull = c("male","female")
            } else
                sexes.to.pull = as.character(obs.data.long[i,"sex"])
            
            M.row.x = array(0,
                            sapply(dim.names,length),
                            dimnames = dim.names)
            
            M.row.x[years.to.pull,ages.to.pull,sexes.to.pull] = 1
            M.row.x = as.numeric(M.row.x)
            
            M = rbind(M,M.row.x)
            
        }
        
        ## For non-population data, use SDS from confidence intervals
        if(calculate.sds.from.ci)
            obs.sds = as.numeric(obs.data.uppers-obs.data.lowers)/2/qnorm(.975)
        else
            obs.sds = sqrt(obs.data.long$value) # for population data, use square root of observed data
        
        if(is.null(obs.data.long$age) & is.null(obs.data.long$sex))
            obs.dimensions = rep("all",nrow(obs.data.long))
        else
            obs.dimensions = paste0(obs.data.long$age,"_",obs.data.long$sex)    
        
        
        rv = list(M=M,
                  obs=obs.data.long$value,
                  obs.year=obs.data.long$year,
                  obs.dimensions=obs.dimensions,
                  obs.sds=obs.sds)
    
    } else # return a null list if this dimension doesn't exist (e.g., year/sex for incidence)
        
        rv = list(M=NULL,
                  obs=NULL,
                  obs.year=NULL,
                  obs.dimensions=NULL,
                  obs.sds=NULL)
    
    rv 
    
}

# Function to actually compute the likelihood 
# Returns density of multivariate normal distribution with mean mu (model simulated value) and covariance matrix that includes 
# both model error and observation error 
compute.likelihood = function(sim,
                              numerator.data.type,
                              denominator.data.type,
                              M, # matrix that maps model strata to observed strata; pre-computed
                              obs.is.proportion,
                              obs.year,
                              obs.dimensions,
                              years,
                              obs, # pre-computed vector of observations 
                              obs.cov.mat, # pre-computed variance-covariance matrix
                              weight,
                              weight.by.age,
                              debug=F){
    
    ## Model component## 
    y.star.with.dim = reshape2::melt(extract.data(sim = sim,
                                                  data.type = numerator.data.type,
                                                  years=years,
                                                  keep.dimensions = c("year","sex","age")))
    y.star = y.star.with.dim$value
    
    if(!is.null(denominator.data.type)) # denominator will be null for population data type
        n.star = as.numeric(extract.data(sim = sim,
                                         data.type = denominator.data.type,
                                         years=years,
                                         keep.dimensions = c("year","sex","age")))
    # mean vector
    mu = M %*% y.star # mean vector with mappings incorporated, matrix multiplication
    
    if(!is.null(denominator.data.type)){
        tau = (y.star*(1-(y.star/n.star))) # covariance vector of np(1-p) 
        cov.mat = M %*% (tau * t(M)) # with mappings incorporated; dim are years x years
    }

    else
        cov.mat = 0 # for population data type only; no model error 
    # will probably have to inflate later using weights 
    
    
    if(obs.is.proportion){
        denominator = as.numeric(M %*% n.star)
        mu = mu/denominator
        D.inverse = diag(1/denominator) # matrix of inverse of diagonal matrix of denominators, can't divide matrices
        cov.mat = D.inverse %*% cov.mat %*% D.inverse
    }
    
    cov.mat = cov.mat + obs.cov.mat # add in observation error part 
    
    if(length(weight)==1)
        cov.mat = cov.mat*(1/weight) # higher weight --> smaller variance; (potentially downweight population likelihood)
    else {
        # if weight isn't a single value, then weight is a named vector of weights where the names are years
        weight.per.obs = weight[as.character(obs.year)]
        
        weight.matrix = sqrt(weight.per.obs) %*% t(sqrt(weight.per.obs))
        cov.mat = cov.mat/weight.matrix
        
    }

    if(!is.null(weight.by.age)){ # named vector e.g., c("80 and over" = 0.5)
        weight.per.obs = sapply(as.character(y.star.with.dim$age),function(age){
            if(is.na(weight.by.age[age]))
                1
            else
                weight.by.age[age]
        })
        
        weight.matrix = sqrt(weight.per.obs) %*% t(sqrt(weight.per.obs))
        cov.mat = cov.mat/weight.matrix
    }
    
    if(debug){
        df = data.frame(year=obs.year,dimension=obs.dimensions,obs=obs,mean=mu,sd=sqrt(diag(cov.mat)))
        df$standard.error = (df$obs-df$mean)/df$sd
        browser()
    }

    # compute and return the density of the multivariate normal
    rv = dmvnorm(x = obs,mean = mu,sigma = cov.mat,log = T)

    
}
