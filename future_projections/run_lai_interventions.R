source('future_projections/create_lai_interventions.R')
#load("future_projections/simset_10_for_lai.Rdata")
load("cached/simset_50_for_lai.Rdata")

# on remote only
# load("mcmc_runs/mcmc_files/merged/mcmc_chains12_south_africa_2025-10-31.Rdata")
# simset = extract.simset(mcmc,
#                         additional.burn=5000,
#                         additional.thin=600) #3000 for 10
# save(simset,file="cached/simset_50_for_lai.Rdata")

set.seed(5678)

print("running no int")
simset.no.int = run.intervention.on.simset(simset,
                                           end.year = 2040,
                                           intervention = NO.INTERVENTION)

print("running ES")
simset.lai.es = run.intervention.on.simset(simset,
                                           end.year = 2040,
                                           intervention = list(lai.from.es,lai.removal.after.5.years))

print("running EU")
simset.lai.eu = run.intervention.on.simset(simset,
                                           end.year = 2040,
                                           intervention = list(lai.from.eu,lai.removal.after.5.years))

print("running DU")
simset.lai.du = run.intervention.on.simset(simset,
                                           end.year = 2040,
                                           intervention = list(lai.from.du,lai.removal.after.5.years))

print("running all, WITHOUT removal")
simset.lai.all.no.removal = run.intervention.on.simset(simset,
                                                       end.year = 2040,
                                                       intervention = lai.from.all)

print("running all, WITH removal after 5 years")
simset.5.year.removal = run.intervention.on.simset(simset,
                                                   end.year = 2040,
                                                   intervention = list(lai.from.all,lai.removal.after.5.years))

print("running all, WITH removal when you turn 25")
simset.age.removal = run.intervention.on.simset(simset,
                                                end.year = 2040,
                                                intervention = list(lai.from.all,lai.removal.by.age))

simset.list.full = list(no.int = simset.no.int,
                        es = simset.lai.es,
                        eu = simset.lai.eu,
                        du = simset.lai.du,
                        all = simset.lai.all.no.removal,
                        rem.by.time = simset.5.year.removal,
                        rem.by.age = simset.age.removal)

print("generating full.results.array")
full.results.array = generate.full.results.array(simset.list = simset.list.full)
infections.averted = calculate.infections.averted(full.results.array,
                                                  interventions = c("es","eu","du","all","rem.by.time","rem.by.age"),
                                                  years = 2022:2030)
totals.on.lai = calculate.totals.on.lai(full.results.array)
totals.on.lai$medians.by.year[,,c("rem.by.time","rem.by.age"),]
totals.on.lai$totals.medians[,c("rem.by.time","rem.by.age"),]

if(1==2){
    simplot(simset.no.int,
            #simset.lai.eu,
            #simset.lai.all.no.removal,
            #simset.5.year.removal,
            #simset.age.removal,
            years=1980:2040, 
            data.types=c("suppression","suppression.oral","suppression.lai"), 
            proportion=T)
    
    simplot(simset.no.int,
            #simset.lai.eu,
            #simset.lai.all.no.removal,
            simset.5.year.removal,
            simset.age.removal,
            years=2020:2040, 
            ages = c("15-24","25 and over"),
            data.types=c("suppression","suppression.oral","suppression.lai"), 
            #data.types=c("suppression.lai"), 
            facet.by=c('age'), 
            proportion=T) #+ geom_vline(xintercept = 2022)

    simplot(simset.no.int,
            #simset.lai.eu,
            #simset.lai.all.no.removal,
            simset.5.year.removal,
            simset.age.removal,
            years=2020:2040, 
            data.types = c("incidence","prevalence"))
    
    simplot(simset.no.int,
            #simset.lai.all.no.removal,
            simset.5.year.removal,
            simset.age.removal,
            facet.by='age', 
            ages = c("15-24","25 and over"),
            years=2020:2040, 
            data.types = c("incidence","prevalence"))
    
    simplot(simset.no.int,
            simset.lai.all.no.removal,
            simset.5.year.removal,
            facet.by=c('age','sex'), 
            ages = "15+",
            years=1980:2040, 
            data.types = c("incidence","prevalence"))
    
    simplot(simset.no.int,
            #simset.lai.all.no.removal,
            #simset.5.year.removal,
            years=1980:2040, 
            data.types=c("suppression","suppression.oral","suppression.lai"), 
            facet.by=c('age','sex'), 
            proportion=T)  
}

