library(bayesian.simulations)

source('model/run_systematic.R')
LOCATION = "South Africa"

print(paste0("loading chain 1"))
#load(paste0("mcmc_runs/mcmc_files/mcmc_unaids.remainder_2025-08-16.Rdata"))
#load(paste0("mcmc_runs/mcmc_files/mcmc_non.unaids.remainder_2025-08-15.Rdata"))
#load(paste0("mcmc_runs/mcmc_files/mcmc_r1.low_2025-08-16.Rdata"))
#load(paste0("mcmc_runs/mcmc_files/mcmc_r1.lower.middle_2025-08-16.Rdata"))
#load(paste0("mcmc_runs/mcmc_files/mcmc_r1.upper.middle_2025-08-16.Rdata"))
#load(paste0("mcmc_runs/mcmc_files/mcmc_mozambique_2025-08-16.Rdata"))
#load(paste0("mcmc_runs/mcmc_files/mcmc_uganda_2025-08-17.Rdata"))
#load(paste0("mcmc_runs/mcmc_files/mcmc_zambia_2025-08-17.Rdata"))
#load(paste0("mcmc_runs/mcmc_files/mcmc_r1.high_2025-08-17.Rdata"))
#load(paste0("mcmc_runs/mcmc_files/mcmc_kenya_2025-08-16.Rdata"))
#load(paste0("mcmc_runs/mcmc_files/mcmc_zimbabwe_2025-08-18.Rdata"))
#load(paste0("mcmc_runs/mcmc_files/mcmc_malawi_2025-08-20.Rdata"))
load(paste0("mcmc_runs/mcmc_files/mcmc_south_africa_chain1_2026-09-02.Rdata"))
mcmc.1 = mcmc

print(paste0("loading chain 2"))
#load(paste0("mcmc_runs/mcmc_files/mcmc_unaids.remainder_chain2_2025-08-21.Rdata"))
#load(paste0("mcmc_runs/mcmc_files/mcmc_non.unaids.remainder_chain2_2025-08-21.Rdata"))
#load(paste0("mcmc_runs/mcmc_files/mcmc_r1.low_chain2_2025-08-21.Rdata"))
#load(paste0("mcmc_runs/mcmc_files/mcmc_r1.lower.middle_chain2_2025-08-21.Rdata"))
#load(paste0("mcmc_runs/mcmc_files/mcmc_r1.upper.middle_chain2_2025-08-21.Rdata"))
#load(paste0("mcmc_runs/mcmc_files/mcmc_mozambique_chain2_2025-08-21.Rdata"))
#load(paste0("mcmc_runs/mcmc_files/mcmc_uganda_chain2_2025-08-22.Rdata"))
#load(paste0("mcmc_runs/mcmc_files/mcmc_zambia_chain2_2025-08-21.Rdata"))
#load(paste0("mcmc_runs/mcmc_files/mcmc_r1.high_chain2_2025-08-22.Rdata"))
#load(paste0("mcmc_runs/mcmc_files/mcmc_kenya_chain2_2025-08-22.Rdata"))
#load(paste0("mcmc_runs/mcmc_files/mcmc_zimbabwe_chain2_2025-08-23.Rdata"))
#load(paste0("mcmc_runs/mcmc_files/mcmc_malawi_chain2_2025-08-24.Rdata"))
load(paste0("mcmc_runs/mcmc_files/mcmc_south_africa_chain2_2026-09-16.Rdata"))
mcmc.2 = mcmc

print(paste0("loading chain 3"))
load(paste0("mcmc_runs/mcmc_files/mcmc_south_africa_chain3_2026-09-27.Rdata"))
mcmc.3 = mcmc

print(paste0("loading chain 4"))
load(paste0("mcmc_runs/mcmc_files/mcmc_south_africa_chain4_2026-10-07.Rdata"))
mcmc.4 = mcmc

mcmc = mcmc.merge.parallel(mcmc.1,mcmc.2,mcmc.3,mcmc.4)

save(mcmc,file=paste0("mcmc_runs/mcmc_files/merged/mcmc_chains1234_",convert_string(LOCATION),"_",Sys.Date(),".Rdata"))

# simset = extract.simset(mcmc,
#                         additional.burn=200,
#                         additional.thin=20)
# 
# save(simset,file=paste0("mcmc_runs/simset_",convert_string(LOCATION),"_",Sys.Date(),".Rdata"))