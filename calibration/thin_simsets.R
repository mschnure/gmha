source("model/run_systematic.R")

#load("mcmc_runs/simset_kenya_2025-02-09.Rdata")
#load("mcmc_runs/simset_south_africa_2025-02-11.Rdata")
load("mcmc_runs/simset_france_2025-02-12.Rdata")

#load("mcmc_runs/mcmc_files/mcmc_kenya_2025-02-09.Rdata") 
#load("mcmc_runs/mcmc_files/mcmc_south_africa_2025-02-11.Rdata")
load("mcmc_runs/mcmc_files/mcmc_france_2025-02-12.Rdata")

# kenya: original simset has 490; burn 7500 and thin 25 to get to 100
# south africa: original simset has 266; burn 4000 and thin 15 to get to 102
# france: original simset has 302; burn 4500 and thin 17 to get to 103
simset = extract.simset(mcmc,
                        additional.burn=4500, # 7500, 4000, 4500
                        additional.thin=17) # 25, 15, 17

#save(simset,file=paste0("mcmc_runs/simset_kenya_thinned_",Sys.Date(),".Rdata"))
#save(simset,file=paste0("mcmc_runs/simset_south_africa_thinned_",Sys.Date(),".Rdata"))
save(simset,file=paste0("mcmc_runs/simset_france_thinned_",Sys.Date(),".Rdata"))
