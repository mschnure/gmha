library(bayesian.simulations)
library(distributions)
library(ggplot2) 
source("model/run_systematic.R")

set.seed(4321) # all runs so far with seed 4321

LOCATION = "France" 

LOCATION.DETAILS = set.likelihood.and.prior.by.location(location=LOCATION)

control = create.adaptive.blockwise.metropolis.control(var.names = LOCATION.DETAILS$prior@var.names,
                                                       simulation.function = SIMULATION.FUNCTION,
                                                       log.prior.distribution = get.density.function(LOCATION.DETAILS$prior,default.log = T),
                                                       log.likelihood = LOCATION.DETAILS$likelihood.to.run,
                                                       var.blocks = PARAMETER.VAR.BLOCKS, # set in calibration/prior_distributions/var_blocks.R
                                                       transformations = LOCATION.DETAILS$transformations,
                                                       initial.covariance.mat = diag((LOCATION.DETAILS$sds/20)^2), # step size
                                                       burn = 0,
                                                       thin = 5)


print(ggplot2::qplot(1,1) + ggplot2::ggtitle(paste0(LOCATION)))

# set starting.values 
mcmc = run.mcmc.with.cache(control = control,
                           n.iter = 30000,
                           starting.values = LOCATION.DETAILS$params.start.values,
                           update.frequency = 100,
                           cache.frequency = 200,
                           cache.dir = file.path("mcmc_cache",convert_string(LOCATION))
)

# mcmc = run.mcmc.from.cache(dir=paste0("mcmc_cache/",convert_string(LOCATION)),
#                            update.frequency = 100)


# run.mcmc.from.cache(dir = "mcmc_cache/")

save(mcmc,file=paste0("mcmc_runs/mcmc_files/mcmc_",convert_string(LOCATION),"_",Sys.Date(),".Rdata"))

simset = extract.simset(mcmc,
                        additional.burn=200,
                        additional.thin=20)

save(simset,file=paste0("mcmc_runs/simset_",convert_string(LOCATION),"_",Sys.Date(),".Rdata"))
