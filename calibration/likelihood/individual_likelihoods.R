
library("mvtnorm")

WEIGHT.YEARS = 1970:2030
WEIGHTS.BY.YEAR = (1/4)^(WEIGHT.YEARS<2010) # before 2010, 1/4x
WEIGHTS.BY.YEAR[WEIGHT.YEARS>=2018] = 4 # from 2018, 4x
names(WEIGHTS.BY.YEAR) = WEIGHT.YEARS

BASE.PARAMETERS.KENYA=create.model.parameters(location = "Kenya")

create.individual.likelihood = function(data.type,
                                        location,
                                        data.manager=DATA.MANAGER,
                                        parameters,
                                        years = 1980:2020,
                                        total.weight = WEIGHTS.BY.YEAR, 
                                        #incidence
                                        incidence.years=years,
                                        incidence.weight=2, 
                                        incidence.obs.correlation=0.5,
                                        incidence.correlation.structure="auto.regressive",
                                        #prevalence
                                        prevalence.years=years,
                                        prevalence.weight=0.25*1, # downweighted due to magnitude; don't want it to outweigh incidence
                                        prevalence.obs.correlation=0.5,
                                        prevalence.correlation.structure="auto.regressive",
                                        #awareness
                                        awareness.years=years,
                                        awareness.weight=1*4.184211, # ratio of points, when accounting for weighting by year (see data_point_weighting)
                                        awareness.obs.correlation=0.5,
                                        awareness.correlation.structure="compound.symmetry",
                                        #engagement
                                        engagement.years=years,
                                        engagement.weight=1*4.184211, # ratio of points, when accounting for weighting by year (see data_point_weighting)
                                        engagement.obs.correlation=0.5,
                                        engagement.correlation.structure="compound.symmetry",
                                        #suppression
                                        suppression.years=years,
                                        suppression.weight=1*9.9375, # ratio of points, when accounting for weighting by year (see data_point_weighting)
                                        suppression.obs.correlation=0.5,
                                        suppression.correlation.structure="compound.symmetry",
                                        #population
                                        population.years=years,
                                        population.weight=1/200000, # have to downweight a lot due to large pop size/number of strata
                                        population.obs.correlation=0.5,
                                        population.correlation.structure="auto.regressive",
                                        #hiv.mortality
                                        hiv.mortality.years=1980:2020,
                                        hiv.mortality.weight=1/256, 
                                        hiv.mortality.obs.correlation=0.5, 
                                        hiv.mortality.correlation.structure="auto.regressive"
){
    
    if(data.type=="incidence"){
        lik = create.likelihood.for.data.type(data.type = "incidence",
                                              data.manager=data.manager,
                                              location = location,
                                              years=incidence.years,
                                              parameters=parameters,
                                              denominator.data.type="population", 
                                              obs.is.proportion=F,
                                              weight=total.weight*incidence.weight,
                                              obs.correlation=incidence.obs.correlation,
                                              correlation.structure=incidence.correlation.structure)
    } else if(data.type=="prevalence"){
        
        lik = create.likelihood.for.data.type(data.type = "prevalence",
                                              data.manager=data.manager,
                                              location = location,
                                              years=prevalence.years,
                                              parameters=parameters,
                                              denominator.data.type="population", 
                                              obs.is.proportion=F,
                                              weight=total.weight*prevalence.weight,
                                              obs.correlation=prevalence.obs.correlation,
                                              correlation.structure=prevalence.correlation.structure)
    } else if(data.type=="awareness"){
        lik = create.likelihood.for.data.type(data.type = "awareness",
                                              data.manager=data.manager,
                                              location = location,
                                              years=awareness.years,
                                              parameters=parameters,
                                              denominator.data.type="prevalence",
                                              obs.is.proportion=T, # awareness data is reported as a proportion 
                                              weight=total.weight*awareness.weight,
                                              obs.correlation=awareness.obs.correlation,
                                              correlation.structure=awareness.correlation.structure)
    } else if (data.type=="engagement"){
        lik = create.likelihood.for.data.type(data.type = "engagement",
                                              data.manager=data.manager,
                                              location = location,
                                              years=engagement.years,
                                              parameters=parameters,
                                              denominator.data.type="awareness", # engagement denominator = awareness
                                              obs.is.proportion=T, # engagement data is reported as a proportion 
                                              weight=total.weight*engagement.weight,
                                              obs.correlation=engagement.obs.correlation,
                                              correlation.structure=engagement.correlation.structure)
    } else if(data.type=="suppression"){
        lik = create.likelihood.for.data.type(data.type = "suppression",
                                              data.manager=data.manager,
                                              location = location,
                                              years=suppression.years,
                                              parameters=parameters,
                                              denominator.data.type="awareness", # suppression denominator = awareness
                                              obs.is.proportion=T, # suppression data is reported as a proportion 
                                              weight=total.weight*suppression.weight,
                                              obs.correlation=suppression.obs.correlation,
                                              correlation.structure=suppression.correlation.structure)
    } else if(data.type=="population"){
        lik = create.likelihood.for.data.type(data.type = "population",
                                              data.manager=data.manager,
                                              location = location,
                                              years=population.years,
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
    } else if(data.type=="hiv.mortality"){
        lik = create.likelihood.for.data.type(data.type = "hiv.mortality",
                                              divide.obs.by.denominator=T,
                                              data.manager=data.manager,
                                              location = location,
                                              years=hiv.mortality.years,
                                              parameters=parameters,
                                              denominator.data.type="prevalence", # technically hiv mortality reported as a number
                                              obs.is.proportion=T, 
                                              weight=total.weight*hiv.mortality.weight,
                                              obs.correlation=hiv.mortality.obs.correlation,
                                              correlation.structure=hiv.mortality.correlation.structure,
                                              use.total=T,
                                              use.sex=F,
                                              use.age=T,
                                              use.age.sex=F)
    } else if (data.type=="awareness.trend"){
        lik = create.likelihood.for.trend(data.type = "awareness",
                                          year.1=2025,
                                          year.2=2030,
                                          probability.of.decrease=.1,
                                          use.strata=F)
    } else
        stop("invalid data type")
    
    components = list(lik = lik)
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
}


incidence.lik.kenya = create.individual.likelihood(data.type = "incidence",
                                             parameters = BASE.PARAMETERS.KENYA,
                                             location = "Kenya")
prev.lik.kenya = create.individual.likelihood(data.type = "prevalence",
                                        parameters = BASE.PARAMETERS.KENYA,
                                        location = "Kenya")
pop.lik.kenya = create.individual.likelihood(data.type = "population",
                                       parameters = BASE.PARAMETERS.KENYA,
                                       location = "Kenya")
aware.lik.kenya = create.individual.likelihood(data.type = "awareness",
                                         parameters = BASE.PARAMETERS.KENYA,
                                         location = "Kenya")
eng.lik.kenya = create.individual.likelihood(data.type = "engagement",
                                       parameters = BASE.PARAMETERS.KENYA,
                                       location = "Kenya")
supp.lik.kenya = create.individual.likelihood(data.type = "suppression",
                                        parameters = BASE.PARAMETERS.KENYA,
                                        location = "Kenya")
hiv.mortality.lik.kenya = create.individual.likelihood(data.type = "hiv.mortality",
                                                 parameters = BASE.PARAMETERS.KENYA,
                                                 location = "Kenya")
aware.trend.lik.kenya = create.individual.likelihood(data.type = "awareness.trend",
                                               parameters = BASE.PARAMETERS.KENYA,
                                               location = "Kenya")

