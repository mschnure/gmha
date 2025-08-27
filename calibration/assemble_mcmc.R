library(bayesian.simulations)

source('model/run_systematic.R')
source('calibration/file_settings.R')
LOCATION = "Tanzania_2"


mcmc = assemble.mcmc.from.cache(file.path(CACHE.NAME,convert_string(LOCATION)),
                                     allow.incomplete = T, chains = 1) 

save(mcmc,file=paste0("mcmc_runs/mcmc_files/mcmc_",convert_string(LOCATION),"_",Sys.Date(),".Rdata"))

simset = extract.simset(mcmc,
                        additional.burn=200,
                        additional.thin=20)

save(simset,file=paste0("mcmc_runs/simset_",convert_string(LOCATION),"_",Sys.Date(),".Rdata"))


#mcmc.merge.parallel()