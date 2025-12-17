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

print("running ES with removal by age")
simset.es = run.intervention.on.simset(simset,
                                       end.year = 2040,
                                       intervention = list(lai.from.es,
                                                           lai.removal.by.age))

print("running EU with removal by age")
simset.eu = run.intervention.on.simset(simset,
                                       end.year = 2040,
                                       intervention = list(lai.from.eu,
                                                           lai.removal.by.age))

print("running DU with removal by age")
simset.du = run.intervention.on.simset(simset,
                                       end.year = 2040,
                                       intervention = list(lai.from.du,
                                                           lai.removal.by.age))

print("running EU RAPID with removal by age")
simset.eu.rapid = run.intervention.on.simset(simset,
                                             end.year = 2040,
                                             intervention = list(lai.from.eu.RAPID,
                                                                 lai.removal.by.age))

print("running DU RAPID with removal by age")
simset.du.rapid = run.intervention.on.simset(simset,
                                             end.year = 2040,
                                             intervention = list(lai.from.du.RAPID,
                                                                 lai.removal.by.age))

print("running all, WITHOUT removal")
simset.no.removal = run.intervention.on.simset(simset,
                                               end.year = 2040,
                                               intervention = lai.from.all)

print("running all, WITH removal by age")
simset.age.removal = run.intervention.on.simset(simset,
                                                end.year = 2040,
                                                intervention = list(lai.from.all,
                                                                    lai.removal.by.age))

print("running all, RAPID, WITHOUT removal")
simset.no.removal.rapid = run.intervention.on.simset(simset,
                                                     end.year = 2040,
                                                     intervention = lai.from.all.rapid)

print("running all, RAPID, WITH removal by age")
simset.age.removal.rapid = run.intervention.on.simset(simset,
                                                      end.year = 2040,
                                                      intervention = list(lai.from.all.rapid,
                                                                          lai.removal.by.age))

simset.list.full = list(no.int = simset.no.int,
                        es = simset.es,
                        eu = simset.eu,
                        du = simset.du,
                        eu.rapid = simset.eu.rapid,
                        du.rapid = simset.du.rapid,
                        all.no.rem = simset.no.removal, 
                        all.age.rem = simset.age.removal,
                        all.no.rem.rapid = simset.no.removal.rapid, 
                        all.age.rem.rapid = simset.age.removal.rapid
                        )

print("generating full.results.array")
full.results.array = generate.full.results.array(simset.list = simset.list.full)

save(full.results.array,file = paste0("cached/nextgen_results_",Sys.Date(),".Rdata"))

infections.averted = calculate.infections.averted(full.results.array,
                                                  interventions = c("es","eu","du",
                                                                    "eu.rapid","du.rapid",
                                                                    "all.no.rem","all.age.rem",
                                                                    "all.no.rem.rapid","all.age.rem.rapid"),
                                                  years = 2022:2030)

if(1==2){
    totals.on.lai = calculate.totals.on.lai(full.results.array)
    totals.on.lai$medians.by.year[,,c("all.no.rem","all.age.rem","all.no.rem.rapid","all.age.rem.rapid"),]
    totals.on.lai$totals.medians[,c("all.no.rem","all.age.rem","all.no.rem.rapid","all.age.rem.rapid"),]
    
    totals.on.lai.no.removal = calculate.totals.on.lai(full.results.array,outcomes = c("lai.art.es","lai.art.eu","lai.art.du"))
    apply(totals.on.lai.no.removal$medians.by.year[,,c("all.no.rem","all.age.rem"),],c("year","intervention","age"),sum)[,,"All ages"]
    apply(totals.on.lai.no.removal$totals.medians[,c("all.no.rem","all.age.rem"),],c("intervention","age"),sum)
    
    # this is the total ever put on LAI
    apply(
        totals.on.lai.no.removal$medians.by.year[,,c("all.no.rem","all.age.rem"),],
        c("intervention", "age"),
        function(x) sum(x[x > 0], na.rm = TRUE)
    )
    
    ### Difference by cohort ###
    (infections.averted["50%","percent.inf.averted",c("es","eu","du")])
    sum(infections.averted["50%","percent.inf.averted",c("es","eu","du")])
    infections.averted["50%","percent.inf.averted",c("all.age.rem")]   
    
    (infections.averted["50%","percent.inf.averted",c("es","eu.rapid","du.rapid")])
    
    # WHY AREN'T THESE TWO THE SAME: 
    sum(infections.averted["50%","percent.inf.averted",c("es","eu.rapid","du.rapid")])
    infections.averted["50%","percent.inf.averted",c("all.age.rem.rapid")]   
}



if(1==2){
    simplot(simset.no.int,
            #simset.lai.eu,
            #simset.lai.all.no.removal,
            #simset.5.year.removal,
            simset.age.removal,
            years=2010:2030, 
            data.types=c("suppression","suppression.oral","suppression.lai"), 
            proportion=T)
    
    simplot(simset.no.int,
            #simset.lai.eu,
            #simset.lai.all.no.removal,
            #simset.5.year.removal,
            simset.age.removal,
            years=2020:2040, 
            ages = c("15-24","25 and over"),
            data.types=c("suppression","suppression.oral","suppression.lai"), 
            #data.types=c("suppression.lai"), 
            facet.by=c('age'), 
            proportion=T) #+ geom_vline(xintercept = 2022)

    simplot(simset.no.int,
            #simset.lai.eu,
            simset.lai.all.no.removal,
            simset.5.year.removal,
            simset.age.removal,
            years=2000:2030, 
            data.types = c("incidence","prevalence"))
    
    simplot(simset.no.int,
            simset.lai.all.no.removal,
            simset.5.year.removal,
            simset.age.removal,
            facet.by='age', 
            ages = c("15-24"),
            years=2000:2030, 
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




## OLD CODE
if(1==2){
    
    # testing to make sure combining individual units was the same as combined unit - it was 
    print("running all, WITHOUT removal, piecewise")
    simset.combined.no.removal = run.intervention.on.simset(simset,
                                                            end.year = 2040,
                                                            intervention = list(lai.from.es,
                                                                                lai.from.eu,
                                                                                lai.from.du))
    
    print("running all, WITH removal by age, piecewise ")
    simset.combined.age.removal = run.intervention.on.simset(simset,
                                                             end.year = 2040,
                                                             intervention = list(lai.from.es,
                                                                                 lai.from.eu,
                                                                                 lai.from.du,
                                                                                 lai.removal.by.age)) 
    
    
    # print("running all, WITH removal after 5 years")
    # simset.5.year.removal = run.intervention.on.simset(simset,
    #                                                    end.year = 2040,
    #                                                    intervention = list(lai.from.all,lai.removal.after.5.years))
    
}

