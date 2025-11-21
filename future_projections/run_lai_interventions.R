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
simset.lai.es = run.intervention.on.simset(simset,
                                           end.year = 2040,
                                           intervention = list(lai.from.es,lai.disengagement))

print("running EU")
simset.lai.eu = run.intervention.on.simset(simset,
                                           end.year = 2040,
                                           intervention = list(lai.from.eu,lai.disengagement))

print("running DU")
simset.lai.du = run.intervention.on.simset(simset,
                                           end.year = 2040,
                                           intervention = list(lai.from.du,lai.disengagement))

print("running all, WITHOUT disengagement")
simset.lai.all = run.intervention.on.simset(simset,
                                            end.year = 2040,
                                            intervention = lai.from.all)

print("running all, WITH disengagement")
simset.lai.all.dis = run.intervention.on.simset(simset,
                                            end.year = 2040,
                                            intervention = list(lai.from.all,lai.disengagement))

simset.list.full = list(no.int = simset.no.int,
                        es = simset.lai.es,
                        eu = simset.lai.eu,
                        du = simset.lai.du,
                        all = simset.lai.all,
                        dis = simset.lai.all.dis)

print("generating full.results.array")
full.results.array = generate.full.results.array(simset.list = simset.list.full)
infections.averted = calculate.infections.averted(full.results.array,
                                                  interventions = c("es","eu","du","all","dis"),
                                                  years = 2022:2030)
if(1==2){
    simplot(simset.no.int,
            simset.lai.eu,
            #simset.lai.all,
            simset.lai.all.dis,
            years=1980:2040, 
            data.types=c("suppression","suppression.oral","suppression.lai"), 
            proportion=T)
    
    simplot(simset.no.int,
            simset.lai.eu,
            #simset.lai.all,
            simset.lai.all.dis,
            years=2020:2040, 
            ages = c("15-24","15+"),
            data.types=c("suppression","suppression.oral","suppression.lai"), 
            facet.by=c('age'), 
            proportion=T)

    simplot(simset.no.int,
            simset.lai.eu,
            #simset.lai.all,
            simset.lai.all.dis,
            years=2020:2040, 
            data.types = c("incidence","prevalence"))
    
    simplot(simset.no.int,
            simset.lai.all,
            simset.lai.all.dis,
            facet.by='age', 
            years=1980:2040, 
            data.types = c("incidence","prevalence"))
    
    simplot(simset.no.int,
            simset.lai.all,
            simset.lai.all.dis,
            facet.by=c('age','sex'), 
            ages = "15+",
            years=1980:2040, 
            data.types = c("incidence","prevalence"))
    
    simplot(simset.no.int,
            #simset.lai.all,
            #simset.lai.all.dis,
            years=1980:2040, 
            data.types=c("suppression","suppression.oral","suppression.lai"), 
            facet.by=c('age','sex'), 
            proportion=T)  
}

