source('future_projections/create_lai_interventions.R')
load("cached/simset_200_for_lai.Rdata")

# on remote only
# load("mcmc_runs/mcmc_files/merged/mcmc_chains12_south_africa_2025-10-31.Rdata")
# simset = extract.simset(mcmc,
#                         additional.burn=5000,
#                         additional.thin=150) #3000 for 10, 600 for 50
# save(simset,file="cached/simset_200_for_lai.Rdata")

set.seed(5678)

print(paste0("1-year coverage = ",PROB.1.YEAR))
print("running no int")
simset.no.int = run.intervention.on.simset(simset,
                                           end.year = 2040,
                                           intervention = NO.INTERVENTION)
print("running ES")
simset.es = run.intervention.on.simset(simset,
                                       end.year = 2040,
                                       intervention = list(lai.from.es,
                                                           lai.removal.by.age))
print("running EU")
simset.eu = run.intervention.on.simset(simset,
                                       end.year = 2040,
                                       intervention = list(lai.from.eu,
                                                           lai.removal.by.age))
print("running DU")
simset.du = run.intervention.on.simset(simset,
                                       end.year = 2040,
                                       intervention = list(lai.from.du,
                                                           lai.removal.by.age))
print("running ES DIRECT")
simset.es.direct = run.intervention.on.simset(simset,
                                             end.year = 2040,
                                             intervention = list(lai.from.es.direct,
                                                                 lai.removal.by.age))

print("running EU DIRECT")
simset.eu.direct = run.intervention.on.simset(simset,
                                             end.year = 2040,
                                             intervention = list(lai.from.eu.direct,
                                                                 lai.removal.by.age))
print("running DU DIRECT")
simset.du.direct = run.intervention.on.simset(simset,
                                             end.year = 2040,
                                             intervention = list(lai.from.du.direct,
                                                                 lai.removal.by.age))
print("running all")
simset.all = run.intervention.on.simset(simset,
                                        end.year = 2040,
                                        intervention = list(lai.from.all,
                                                            lai.removal.by.age))
print("running all DIRECT")
simset.all.direct = run.intervention.on.simset(simset,
                                              end.year = 2040,
                                              intervention = list(lai.from.all.direct,
                                                                  lai.removal.by.age))

save(simset.no.int, file = paste0("cached/simset.noint_",(PROB.1.YEAR*100),"_1yr_",Sys.Date(),".Rdata"))
save(simset.es, file = paste0("cached/simset.es_",(PROB.1.YEAR*100),"_1yr_",Sys.Date(),".Rdata"))
save(simset.eu, file = paste0("cached/simset.eu_",(PROB.1.YEAR*100),"_1yr_",Sys.Date(),".Rdata"))
save(simset.du, file = paste0("cached/simset.du_",(PROB.1.YEAR*100),"_1yr_",Sys.Date(),".Rdata"))
save(simset.es.direct, file = paste0("cached/simset.es.direct_",(PROB.1.YEAR*100),"_1yr_",Sys.Date(),".Rdata"))
save(simset.eu.direct, file = paste0("cached/simset.eu.direct_",(PROB.1.YEAR*100),"_1yr_",Sys.Date(),".Rdata"))
save(simset.du.direct, file = paste0("cached/simset.du.direct_",(PROB.1.YEAR*100),"_1yr_",Sys.Date(),".Rdata"))
save(simset.all, file = paste0("cached/simset.all_",(PROB.1.YEAR*100),"_1yr_",Sys.Date(),".Rdata"))
save(simset.all.direct, file = paste0("cached/simset.all.direct_",(PROB.1.YEAR*100),"_1yr_",Sys.Date(),".Rdata"))

if(1==2){
  save(simset.no.int, file = paste0("cached/simset.noint_",(PROB.5.YEAR*100),"_",Sys.Date(),".Rdata"))
  save(simset.es, file = paste0("cached/simset.es_",(PROB.5.YEAR*100),"_",Sys.Date(),".Rdata"))
  save(simset.eu, file = paste0("cached/simset.eu_",(PROB.5.YEAR*100),"_",Sys.Date(),".Rdata"))
  save(simset.du, file = paste0("cached/simset.du_",(PROB.5.YEAR*100),"_",Sys.Date(),".Rdata"))
  save(simset.es.direct, file = paste0("cached/simset.es.direct_",(PROB.5.YEAR*100),"_",Sys.Date(),".Rdata"))
  save(simset.eu.direct, file = paste0("cached/simset.eu.direct_",(PROB.5.YEAR*100),"_",Sys.Date(),".Rdata"))
  save(simset.du.direct, file = paste0("cached/simset.du.direct_",(PROB.5.YEAR*100),"_",Sys.Date(),".Rdata"))
  save(simset.all, file = paste0("cached/simset.all_",(PROB.5.YEAR*100),"_",Sys.Date(),".Rdata"))
  save(simset.all.direct, file = paste0("cached/simset.all.direct_",(PROB.5.YEAR*100),"_",Sys.Date(),".Rdata"))
}

