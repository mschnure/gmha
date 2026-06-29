
LOCATION = "r1.high"

mcmc = assemble.mcmc.from.cache(file.path("mcmc_cache",paste0(convert_string(LOCATION),"_",CHAIN)),
                                allow.incomplete = T, chains = 1) 

# original: 50,000, burn 200 and thin by 20 to get to 2490
# simset = extract.simset(mcmc,
#                         additional.burn=200,
#                         additional.thin=20)

# new: 50,000, burn 25,000 and thin by 250 to get to 100
simset = extract.simset(mcmc,
                        additional.burn=25000,
                        additional.thin=250)

save(simset,file=paste0("mcmc_runs/simset_",convert_string(LOCATION),"_chain",CHAIN,"_",Sys.Date(),".Rdata")) 


# Ran projections gain but thinning didn't help