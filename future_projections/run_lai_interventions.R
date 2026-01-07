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
print("running EU RAPID")
simset.eu.rapid = run.intervention.on.simset(simset,
                                             end.year = 2040,
                                             intervention = list(lai.from.eu.RAPID,
                                                                 lai.removal.by.age))
print("running DU RAPID")
simset.du.rapid = run.intervention.on.simset(simset,
                                             end.year = 2040,
                                             intervention = list(lai.from.du.RAPID,
                                                                 lai.removal.by.age))
print("running all")
simset.all = run.intervention.on.simset(simset,
                                        end.year = 2040,
                                        intervention = list(lai.from.all,
                                                            lai.removal.by.age))
print("running all RAPID")
simset.all.rapid = run.intervention.on.simset(simset,
                                              end.year = 2040,
                                              intervention = list(lai.from.all.rapid,
                                                                  lai.removal.by.age))
print("running combined, EU RAPID only")
simset.comb.eu.rapid = run.intervention.on.simset(simset,
                                                  end.year = 2040,
                                                  intervention = list(lai.from.es,
                                                                      lai.from.eu.RAPID,
                                                                      lai.from.du,
                                                                      lai.removal.by.age))
print("running combined, DU RAPID only")
simset.comb.du.rapid = run.intervention.on.simset(simset,
                                                  end.year = 2040,
                                                  intervention = list(lai.from.es,
                                                                      lai.from.eu,
                                                                      lai.from.du.RAPID,
                                                                      lai.removal.by.age))

simset.list.full = list(no.int = simset.no.int,
                        es = simset.es,
                        eu = simset.eu,
                        du = simset.du,
                        eu.rapid = simset.eu.rapid,
                        du.rapid = simset.du.rapid,
                        all = simset.all,
                        all.rapid = simset.all.rapid,
                        all.eu.rapid = simset.comb.eu.rapid,
                        all.du.rapid = simset.comb.du.rapid
                        )

print("generating full.results.array")
full.results.array = generate.full.results.array(simset.list = simset.list.full)

save(simset.list.full,full.results.array,file = paste0("cached/nextgen_results_",Sys.Date(),".Rdata"))

infections.averted = calculate.infections.averted(full.results.array,
                                                  interventions = c("es","eu","du",
                                                                    "eu.rapid","du.rapid",
                                                                    "all","all.rapid",
                                                                    "all.eu.rapid","all.du.rapid"),
                                                  years = 2022:2030)

write.csv(infections.averted, file = paste0("cached/nextgen_infections_averted_",Sys.Date(),".csv"))

if(1==2){
    #interventions = c("all","all.rapid")
    #interventions = c("all.eu.rapid","all.du.rapid")
    #interventions = c("all","all.rapid","all.eu.rapid","all.du.rapid")
    interventions = c("es","eu","du","eu.rapid","du.rapid")
    
    totals.on.lai = calculate.totals.on.lai(full.results.array, outcomes = c("lai.art.es","lai.art.eu","lai.art.du"))
    #totals.on.lai = calculate.totals.on.lai(full.results.array, outcomes = c("lai.art.es","lai.art.eu","lai.art.du"),estimate = "lower")
    #totals.on.lai = calculate.totals.on.lai(full.results.array, outcomes = c("lai.art.es","lai.art.eu","lai.art.du"),estimate = "upper")
    totals.on.lai$medians.by.year[as.character(2021:2028),,interventions,"All ages"] 
    #totals.on.lai$totals.medians[,interventions,"All ages"] # this one doesn't match exactly - use the applies below instead 
    
    # this is the total ever put on LAI
    apply(
        totals.on.lai$medians.by.year[,,interventions,"All ages"],
        c("intervention"),
        function(x) sum(x[x > 0], na.rm = TRUE)
    )
    
    # by cohort 
    apply(
        totals.on.lai$medians.by.year[,,interventions,"All ages"],
        c("outcome","intervention"),
        function(x) sum(x[x > 0], na.rm = TRUE)
    )
    
    
}



if(1==2){
    simplot(simset.no.int,
            #simset.age.removal,
            #simset.age.removal.rapid,
            years=2010:2030, 
            data.types=c("suppression","suppression.oral","suppression.lai"), 
            proportion=T)
    
    simplot(simset.no.int,
            #simset.age.removal,
            simset.age.removal.rapid,
            #simset.du,
            #simset.du.rapid,
            years=2020:2040, 
            ages = c("15-24","25 and over"),
            data.types=c("suppression","suppression.oral","suppression.lai"), 
            facet.by=c('age'), 
            proportion=T)
    
    simplot(simset.no.int,
            #simset.lai.eu,
            simset.lai.all.no.removal,
            simset.5.year.removal,
            simset.age.removal,
            years=2000:2030, 
            data.types = c("incidence","prevalence"))
    
    simplot(simset.no.int,
            #simset.age.removal,
            simset.age.removal.rapid,
            #simset.du,
            simset.es,
            simset.eu.rapid,
            simset.du.rapid,
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
    
    ### Difference by cohort ###
    (infections.averted["50%","percent.inf.averted",c("es","eu","du")])
    sum(infections.averted["50%","percent.inf.averted",c("es","eu","du")])
    infections.averted["50%","percent.inf.averted",c("all.age.rem")]   
    
    (infections.averted["50%","percent.inf.averted",c("es","eu.rapid","du.rapid")])
    
    # WHY AREN'T THESE TWO THE SAME
    # I think there's competition between the arrows: people who are captured in ES alone might now be captured sooner in DU or EU rapid
    # When you add the individual interventions, it double counts those people, essentially
    
    # what I need to do is break out the tracking within the combined intervention - see where they came from 
    sum(infections.averted["50%","percent.inf.averted",c("es","eu.rapid","du.rapid")])
    infections.averted["50%","percent.inf.averted",c("all.age.rem.rapid")]   
    
    # You can see that the rapid scenario makes more people join LAI via EU and DU 
    (totals.on.lai.no.removal$medians.by.year[as.character(2021:2030),,c("all.age.rem"),"All ages"])
    (totals.on.lai.no.removal$medians.by.year[as.character(2021:2030),,c("all.age.rem.rapid"),"All ages"])
    
    
    
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
    
    # print("running all, WITHOUT removal")
    # simset.no.removal = run.intervention.on.simset(simset,
    #                                                end.year = 2040,
    #                                                intervention = lai.from.all)
    
    
    # print("running all, RAPID, WITHOUT removal")
    # simset.no.removal.rapid = run.intervention.on.simset(simset,
    #                                                      end.year = 2040,
    #                                                      intervention = lai.from.all.rapid)
    
    
    # print("running all, WITH removal after 5 years")
    # simset.5.year.removal = run.intervention.on.simset(simset,
    #                                                    end.year = 2040,
    #                                                    intervention = list(lai.from.all,lai.removal.after.5.years))
    
}

