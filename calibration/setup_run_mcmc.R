library(bayesian.simulations)
library(distributions)
library(ggplot2) 
source("model/run_systematic.R")

CHAIN = 2

set.seed(4321*CHAIN)
# All countries, 8/04 - 4321
# 8/18: updated with *CHAIN 

LOCATION = "Tanzania" 
RESUME.RUNNING = F
RESUME.RUNNING.WITH.CHAIN = T
WEIGHTED.PREVALENCE = F # if set to T, will run with 4x prevalence weight 
N.ITER = 100000

if(!RESUME.RUNNING & !RESUME.RUNNING.WITH.CHAIN){
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
  
  
  print(ggplot2::qplot(1,1) + ggplot2::ggtitle(paste0(LOCATION,": chain ",CHAIN)))
  
  # set starting.values 
  mcmc = run.mcmc.with.cache(control = control,
                             n.iter = N.ITER,
                             starting.values = LOCATION.DETAILS$params.start.values,
                             update.frequency = 100,
                             cache.frequency = 200,
                             cache.dir = file.path("mcmc_cache",paste0(convert_string(LOCATION),"_",CHAIN))
  )
  
  save(mcmc,file=paste0("mcmc_runs/mcmc_files/mcmc_",convert_string(LOCATION),"_chain",CHAIN,"_",Sys.Date(),".Rdata"))
  
  simset = extract.simset(mcmc,
                          additional.burn=200,
                          additional.thin=20)
  
  save(simset,file=paste0("mcmc_runs/simset_",convert_string(LOCATION),"_chain",CHAIN,"_",Sys.Date(),".Rdata")) 
  
}

if(RESUME.RUNNING){
    print(ggplot2::qplot(1,1) + ggplot2::ggtitle(paste0(LOCATION)))
    mcmc = run.mcmc.from.cache(dir=paste0("mcmc_cache/",convert_string(LOCATION)),
                               update.frequency = 100)
    
    # once all of chain 1 is done, convert to this:   
    # print(ggplot2::qplot(1,1) + ggplot2::ggtitle(paste0(LOCATION,": chain ",CHAIN)))
    # mcmc = run.mcmc.from.cache(dir=file.path("mcmc_cache",paste0(convert_string(LOCATION),"_",CHAIN)),
    #                            update.frequency = 100)
    
    # and put this outside of the if(RESUME.RUNNING) statement
    save(mcmc,file=paste0("mcmc_runs/mcmc_files/mcmc_",convert_string(LOCATION),"_",Sys.Date(),".Rdata"))
    
    simset = extract.simset(mcmc,
                            additional.burn=200,
                            additional.thin=20)
    
    save(simset,file=paste0("mcmc_runs/simset_",convert_string(LOCATION),"_",Sys.Date(),".Rdata"))
}

if(RESUME.RUNNING.WITH.CHAIN){
  # once all of chain 1 is done, convert to this:   
  print(ggplot2::qplot(1,1) + ggplot2::ggtitle(paste0(LOCATION,": chain ",CHAIN)))
  mcmc = run.mcmc.from.cache(dir=file.path("mcmc_cache",paste0(convert_string(LOCATION),"_",CHAIN)),
                             update.frequency = 100)
  
  # and put this outside of the if(RESUME.RUNNING) statement
  save(mcmc,file=paste0("mcmc_runs/mcmc_files/mcmc_",convert_string(LOCATION),"_chain",CHAIN,"_",Sys.Date(),".Rdata"))
  
  simset = extract.simset(mcmc,
                          additional.burn=200,
                          additional.thin=20)
  
  save(simset,file=paste0("mcmc_runs/simset_",convert_string(LOCATION),"_chain",CHAIN,"_",Sys.Date(),".Rdata")) 
}

