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
#load(paste0("mcmc_runs/mcmc_files/mcmc_south_africa_2025-08-21.Rdata"))
load(paste0("mcmc_runs/mcmc_files/mcmc_south_africa_youth.supp_1_youth.supp_chain1_2025-10-30.Rdata"))
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
#load(paste0("mcmc_runs/mcmc_files/mcmc_south_africa_chain2_2025-08-26.Rdata"))
load(paste0("mcmc_runs/mcmc_files/mcmc_south_africa_youth.supp_2_youth.supp_chain2_2025-10-30.Rdata"))
mcmc.2 = mcmc

mcmc = mcmc.merge.parallel(mcmc.1,mcmc.2)

save(mcmc,file=paste0("mcmc_runs/mcmc_files/merged/mcmc_chains12_",convert_string(LOCATION),"_",Sys.Date(),".Rdata"))

# simset = extract.simset(mcmc,
#                         additional.burn=200,
#                         additional.thin=20)
# 
# save(simset,file=paste0("mcmc_runs/simset_",convert_string(LOCATION),"_",Sys.Date(),".Rdata"))