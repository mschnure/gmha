library(bayesian.simulations)
library(distributions)
library(ggplot2) 
source("model/run_systematic.R")

CHAIN = 1
MCMC.CODE = 'youth.supp' # will paste this in the cache and mcmc files, use "" for original version (I think)

set.seed(4321*CHAIN)
# All countries, 8/04 - 4321
# 8/18: updated with *CHAIN 

LOCATION = "South Africa" 

# weight/iteration options 
{
    WEIGHTED.PREVALENCE = F # if set to T, will run with 4x prevalence weight 
    N.ITER = 30000
}

# resume running option
RESUME.RUNNING = F

# if a fresh run (i.e., not resuming)
if(!RESUME.RUNNING){
    LOCATION.DETAILS = set.likelihood.and.prior.by.location(location=LOCATION,
                                                            weighted.prevalence = WEIGHTED.PREVALENCE)
  
  control = create.adaptive.blockwise.metropolis.control(var.names = LOCATION.DETAILS$prior@var.names,
                                                         simulation.function = SIMULATION.FUNCTION,
                                                         log.prior.distribution = get.density.function(LOCATION.DETAILS$prior,default.log = T),
                                                         log.likelihood = LOCATION.DETAILS$likelihood.to.run,
                                                         var.blocks = PARAMETER.VAR.BLOCKS, # set in calibration/prior_distributions/var_blocks.R
                                                         transformations = LOCATION.DETAILS$transformations,
                                                         initial.covariance.mat = diag((LOCATION.DETAILS$sds/20)^2), # step size
                                                         burn = 0,
                                                         thin = 5)
  
  
  print(ggplot2::qplot(1,1) + ggplot2::ggtitle(paste0(LOCATION,": chain ",CHAIN, "; mcmc code: ",MCMC.CODE)))
  
  # set starting.values 
  mcmc = run.mcmc.with.cache(control = control,
                             n.iter = N.ITER,
                             starting.values = LOCATION.DETAILS$params.start.values,
                             update.frequency = 100,
                             cache.frequency = 200,
                             cache.dir = file.path("mcmc_cache",paste0(convert_string(LOCATION),"_",MCMC.CODE,"_",CHAIN))
  )
  
  save(mcmc,file=paste0("mcmc_runs/mcmc_files/mcmc_",convert_string(LOCATION),"_",MCMC.CODE,"_chain",CHAIN,"_",Sys.Date(),".Rdata"))
  
  simset = extract.simset(mcmc,
                          additional.burn=200,
                          additional.thin=20)
  
  save(simset,file=paste0("mcmc_runs/simset_",convert_string(LOCATION),"_",MCMC.CODE,"_chain",CHAIN,"_",Sys.Date(),".Rdata")) 
  
}

if(RESUME.RUNNING){
    print(ggplot2::qplot(1,1) + ggplot2::ggtitle(paste0(LOCATION,": chain ",CHAIN)))
    mcmc = run.mcmc.from.cache(dir=file.path("mcmc_cache",paste0(convert_string(LOCATION),"_",CHAIN)),
                               update.frequency = 100)
    
    save(mcmc,file=paste0("mcmc_runs/mcmc_files/mcmc_",convert_string(LOCATION),"_chain",CHAIN,"_",Sys.Date(),".Rdata"))
    
    simset = extract.simset(mcmc,
                            additional.burn=200,
                            additional.thin=20)
    
    save(simset,file=paste0("mcmc_runs/simset_",convert_string(LOCATION),"_chain",CHAIN,"_",Sys.Date(),".Rdata")) 
}

