
source('calibration/file_settings.R')

library(bayesian.simulations)

# original 
#mcmc.test = assemble.mcmc.from.cache(file.path(MCMC.CACHE.DIR, CACHE.NAME), allow.incomplete = T, chains = 1) # 

mcmc.test = assemble.mcmc.from.cache(file.path(CACHE.NAME), allow.incomplete = T, chains = 1) 


# original
#save(mcmc.32,file=file.path(MCMC.DIR, paste0("mcmc_v",MCMC.VERSION,"_", Sys.Date(), ".Rdata")))

save(mcmc.test,file=file.path(MCMC.SAVE.DIR, paste0("mcmc_test_", Sys.Date(), ".Rdata")))
