# library(devtools)
# install_github("tfojo1/bayesian.simulations")
# install_github("tfojo1/distributions")
library(bayesian.simulations)
library(distributions)
library(ggplot2) 
source("model/run_systematic.R")

set.seed(4321) # kenya 12/17 ran with seed 4321

KENYA.LIK = create.likelihood(parameters = create.model.parameters(location = "Kenya"),
                              location="Kenya")
SOUTH.AFRICA.LIK = create.likelihood(parameters = create.model.parameters(location = "South Africa"),
                                     location="South Africa")

LOCATION = "South Africa"
LOCATION.FOR.SAVING = convert_string(LOCATION)
LIKELIHOOD.TO.RUN = SOUTH.AFRICA.LIK 

# PRIOR is set to country-specific prior at the end of prior_distributions.R - CHANGE FOR EACH COUNTRY
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
                                n.iter = 5000,
                                starting.values = params.start.values, 
                                update.frequency = 5,
                                cache.frequency = 200,
                                cache.dir = file.path("mcmc_cache",LOCATION.FOR.SAVING)
)

# mcmc.test = run.mcmc.from.cache(dir="mcmc_cache",
#                            update.frequency = 5)


# run.mcmc.from.cache(dir = "mcmc_cache/")

save(mcmc.test,file=paste0("mcmc_runs/mcmc_test_",LOCATION.FOR.SAVING,"_",Sys.Date(),".Rdata"))
#save(mcmc.test,file="mcmc_runs/mcmc_test_2024-12-17.Rdata")
