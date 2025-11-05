source('future_projections/create_lai_interventions.R')
load("future_projections/simset_10_for_lai.Rdata")

# on remote only
# load("mcmc_runs/mcmc_files/merged/mcmc_chains12_south_africa_2025-10-31.Rdata")
# simset = extract.simset(mcmc,
#                         additional.burn=5000,
#                         additional.thin=3000)
# save(simset,file="future_projections/simset_10_for_lai.Rdata")


print("running no int")
simset.no.int = run.intervention.on.simset(simset,
                                           end.year = 2040,
                                           intervention = NO.INTERVENTION)

print("running ES")
simset.lai.es.10 = run.intervention.on.simset(simset,
                                              end.year = 2040,
                                              intervention = lai.10.from.es)

print("running EU")
simset.lai.eu.10 = run.intervention.on.simset(simset,
                                              end.year = 2040,
                                              intervention = lai.10.from.eu)

print("running DU")
simset.lai.du.10 = run.intervention.on.simset(simset,
                                              end.year = 2040,
                                              intervention = lai.10.from.du)

print("running all")
simset.lai.all.10 = run.intervention.on.simset(simset,
                                               end.year = 2040,
                                               intervention = lai.10.from.all)

simplot(simset.no.int,
        #simset.lai.es.10,
        #simset.lai.eu.10,
        #simset.lai.du.10,
        #simset.lai.all.10,
        years=1980:2040, 
        data.types=c("suppression","suppression.oral","suppression.lai"), 
        proportion=T) + geom_hline(y=0.91)

simplot(simset.no.int,
        #simset.lai.es.10,
        #simset.lai.eu.10,
        #simset.lai.du.10,
        simset.lai.all.10,
        years=1980:2040, 
        data.types = c("incidence","prevalence"))

simplot(simset.no.int,
        #simset.lai.es.10,
        #simset.lai.eu.10,
        #simset.lai.du.10,
        simset.lai.all.10,
        years=1980:2040, 
        data.types=c("suppression","suppression.oral","suppression.lai"), 
        facet.by=c('age','sex'), 
        proportion=T)
