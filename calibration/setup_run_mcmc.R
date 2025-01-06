library(bayesian.simulations)
library(distributions)
library(ggplot2) 
source("model/run_systematic.R")

set.seed(4321) # kenya 12/17, 12/31 ran with seed 4321

KENYA.LIK = create.likelihood(parameters = create.model.parameters(location = "Kenya"),
                              location="Kenya")
SOUTH.AFRICA.LIK = create.likelihood(parameters = create.model.parameters(location = "South Africa"),
                                     location="South Africa")
FRANCE.LIK = create.likelihood(parameters = create.model.parameters(location = "France"),
                               location="France")

#### CHANGE THESE TWO LINES (and the last line of prior_distributions) WHEN RUNNING A NEW COUNTRY ####
LOCATION = "France" 
LIKELIHOOD.TO.RUN = FRANCE.LIK 

# PRIOR is set to country-specific prior at the end of prior_distributions.R - CHANGE FOR EACH COUNTRY 
# (for now, using Kenya for all)
control = create.adaptive.blockwise.metropolis.control(var.names = PRIOR@var.names,
                                                       simulation.function = SIMULATION.FUNCTION,
                                                       log.prior.distribution = get.density.function(PRIOR,default.log = T),
                                                       log.likelihood = LIKELIHOOD.TO.RUN,
                                                       var.blocks = PARAMETER.VAR.BLOCKS,
                                                       transformations = TRANSFORMATIONS,
                                                       initial.covariance.mat = diag((SDS/20)^2), # step size
                                                       burn = 0,
                                                       thin = 5) 

# set starting.values 
mcmc.test = run.mcmc.with.cache(control = control,
                                n.iter = 10000,
                                starting.values = params.start.values, 
                                update.frequency = 5,
                                cache.frequency = 200,
                                cache.dir = file.path("mcmc_cache",convert_string(LOCATION))
)

# mcmc.test = run.mcmc.from.cache(dir="mcmc_cache",
#                            update.frequency = 5)


# run.mcmc.from.cache(dir = "mcmc_cache/")

save(mcmc.test,file=paste0("mcmc_runs/mcmc_test_",convert_string(LOCATION),"_",Sys.Date(),".Rdata"))

simset.test = extract.simset(mcmc.test,
                             additional.burn=200,
                             additional.thin=20)

save(simset.test,file=paste0("mcmc_runs/simset.test_",convert_string(LOCATION),"_",Sys.Date(),".Rdata"))
