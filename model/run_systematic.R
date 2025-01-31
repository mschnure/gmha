################################################################################################
# Description: Code that runs from initializing parameters all the way through simulation
################################################################################################

# Functions 
#     1. run.model.for.parameters 
#     2. extract.hiv.data.for.ncd 

source('source_code.R')
library(bayesian.simulations)


## Added this in so that parameters objects exists without running a sim - need this for likelihood functions 
if(1==2){
    parameters = create.model.parameters(location=LOCATION)
    parameters = map.model.parameters(parameters,location = LOCATION)
}

# Function to call in MCMC - calls run.model.for.parameters
SIMULATION.FUNCTION = function(sampled.parameters){
    run.model.for.parameters(variable.parameters = sampled.parameters,
                             parameters = create.model.parameters(location = LOCATION),
                             location = LOCATION)
}


# Single function that is analogous to all the code in the test_case file, but also allows for
# easier manipulation of sampled parameters
#     1. Creates basic model parameters; maps all parameters to structure needed for diffeq;
#         sets up the initial state; runs the model
#     2. Can be passed variable.parameters as an input to override default sampled parameters
#     3. Called in parameter_optim code

## Run model with sampled parameters
run.model.for.parameters = function(location,
                                    variable.parameters,
                                    parameters=create.model.parameters(location=location),
                                    start.year=1970,
                                    end.year=2031,
                                    interventions=NO.INTERVENTION){
    
    sampled.parameters = get.default.parameters(location=location)
    
    invalid.names = setdiff(names(variable.parameters), names(sampled.parameters))
    if(length(invalid.names)>0)
        stop(paste0("Invalid parameters passed: ",paste0(invalid.names,collapse = ", ")))
    
    sampled.parameters[names(variable.parameters)]=variable.parameters #overwrite with whatever new parameters we want
    
    parameters = map.model.parameters(parameters,
                                      location=location,
                                      sampled.parameters=sampled.parameters,
                                      interventions=interventions)
    
    state.dim.names = list(age=parameters$AGES, 
                           sex=parameters$SEXES,
                           subgroup=parameters$SUBGROUPS,
                           hiv.status=parameters$HIV.STATUS)
    
    initial.state = get.initial.population(year = "1970", 
                                           location=location,
                                           data.manager = DATA.MANAGER, 
                                           parameters = parameters,
                                           model.age.cutoffs = MODEL.AGE.CUTOFFS, 
                                           ages = parameters$AGES, 
                                           sexes = parameters$SEXES, 
                                           seed.to.ages = c(5,6,7,8), 
                                           seed.to.sexes = c(1,2), 
                                           seed.n = 1)
    
    #save(parameters,variable.parameters,file="calibration/debug.parameters.Rdata")
    
    sim = run.model(parameters=parameters,
                    initial.state=initial.state,
                    start.year=start.year, # later make these arguments that I pass to the function, with these as defaults 
                    end.year=end.year,
                    keep.years=c(start.year:end.year),
                    location=location)
    
    sim$parameters = parameters
    
    sim
    
}

