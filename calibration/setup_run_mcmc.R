library(bayesian.simulations)
library(distributions)
library(ggplot2) 
source("model/run_systematic.R")

CHAIN = 1

set.seed(1010*CHAIN)
# All countries, 5/29 - 1234

LOCATION = "Zimbabwe"
RESUME.RUNNING = F
WEIGHTED.PREVALENCE = T # if set to T, will run with 4x prevalence weight 
TOTAL.MORTALITY.WEIGHT = 1
N.ITER = 50000
TOTAL.WEIGHT = 1

print(paste0("Running MCMC with ",N.ITER," iterations, with total weight = ",TOTAL.WEIGHT,
             ", weighted prevalence set to ",WEIGHTED.PREVALENCE,
             " and total mortality weight set to ",TOTAL.MORTALITY.WEIGHT))

if(!RESUME.RUNNING){
  LOCATION.DETAILS = set.likelihood.and.prior.by.location(location=LOCATION,
                                                          total.weight = TOTAL.WEIGHT,
                                                          weighted.prevalence = WEIGHTED.PREVALENCE,
                                                          total.mortality.weight = TOTAL.MORTALITY.WEIGHT)
  
  control = create.adaptive.blockwise.metropolis.control(var.names = LOCATION.DETAILS$prior@var.names,
                                                         simulation.function = SIMULATION.FUNCTION,
                                                         log.prior.distribution = get.density.function(LOCATION.DETAILS$prior,default.log = T),
                                                         log.likelihood = LOCATION.DETAILS$likelihood.to.run,
                                                         var.blocks = PARAMETER.VAR.BLOCKS, # set in calibration/prior_distributions/var_blocks.R
                                                         transformations = LOCATION.DETAILS$transformations,
                                                         initial.covariance.mat = diag((LOCATION.DETAILS$sds/20)^2), # step size
                                                         burn = 0,
                                                         thin = 5)
  
  
  print(ggplot2::qplot(1,1) + ggplot2::ggtitle(paste0(LOCATION,": chain ",CHAIN)))
  
  # set starting.values 
  mcmc = run.mcmc.with.cache(control = control,
                             n.iter = N.ITER,
                             starting.values = LOCATION.DETAILS$params.start.values,
                             update.frequency = 100,
                             cache.frequency = 200,
                             cache.dir = file.path("mcmc_cache",paste0(convert_string(LOCATION),"_",CHAIN)))
  
}

if(RESUME.RUNNING){
  print(ggplot2::qplot(1,1) + ggplot2::ggtitle(paste0(LOCATION,": chain ",CHAIN)))
  mcmc = run.mcmc.from.cache(dir=file.path("mcmc_cache",paste0(convert_string(LOCATION),"_",CHAIN)),
                             update.frequency = 100)
  
}



# SAVE MCMC AND SIMSET
mcmc = assemble.mcmc.from.cache(file.path("mcmc_cache",paste0(convert_string(LOCATION),"_",CHAIN)),
                                allow.incomplete = T, chains = 1) 

save(mcmc,file=paste0("mcmc_runs/mcmc_files/mcmc_",convert_string(LOCATION),"_chain",CHAIN,"_",Sys.Date(),".Rdata"))

simset = extract.simset(mcmc,
                        additional.burn=200,
                        additional.thin=20)

save(simset,file=paste0("mcmc_runs/simset_",convert_string(LOCATION),"_chain",CHAIN,"_",Sys.Date(),".Rdata")) 



