library(colorspace)
source('future_projections/create_lai_interventions.R')

# only run if you need to generate new results array - takes a while. otherwise start from loading below 
if(1==2){
    source('future_projections/load_lai_results.R')
    
    full.results.array = generate.full.results.array(simset.list = simset.list.full)
    dimnames(full.results.array)$intervention = paste0(
        dimnames(full.results.array)$intervention,
        rep(c(".25", ".100"), each = length(unique(dimnames(full.results.array)$intervention)))
    )
    
    dimnames(full.results.array)$intervention[1] = "no.int"
    
    save(full.results.array, file = paste0("cached/full.results.array_",Sys.Date(),".Rdata"))
    
    
}

load("cached/full.results.array_2026-01-22.Rdata")


YEARS.TO.SUMMARIZE = 2022:2030
INTERVENTIONS = dimnames(full.results.array)$intervention[c(-1,-10)] # remove no intervention

infections.averted = calculate.infections.averted(full.results.array,
                                                  output = "number",
                                                  interventions = INTERVENTIONS,
                                                  years = YEARS.TO.SUMMARIZE)

infections.averted.15.to.24 = calculate.infections.averted(full.results.array,
                                                           output = "number",
                                                           ages = c("15-19","20-24"),
                                                           interventions = INTERVENTIONS,
                                                           years = YEARS.TO.SUMMARIZE)

#table(is.na(infections.averted))

initiated.on.lai = full.results.array[as.character(YEARS.TO.SUMMARIZE),,,c("lai.art.es","lai.art.eu","lai.art.du"),,INTERVENTIONS, drop = F]
initiated.on.lai = apply(initiated.on.lai,c("outcome","sim","intervention"),sum)
initiated.on.lai = aperm(initiated.on.lai, c("sim", "intervention", "outcome"))
initiated.on.lai.total = apply(initiated.on.lai,c("sim","intervention"),sum)

dim.names = c(dimnames(initiated.on.lai)[1:2],
              list("outcome" = c(dimnames(initiated.on.lai)$outcome,"total")))

initiated.on.lai = array(c(initiated.on.lai,initiated.on.lai.total),
                         dim = sapply(dim.names,length),
                         dimnames = dim.names)


# Numbers initiated on LAI by intervention and arrow 
if(1==2){
    lai.by.arrow = apply(initiated.on.lai,c("outcome","intervention"),quantile,probs = c(0.025,0.5,0.975),na.rm = T)
    #write.csv(lai.by.arrow, file = paste0("cached/lai.by.arrow_",Sys.Date(),".csv"))
    
    # LAI per infection averted 
    initiated.per.infection.averted = initiated.on.lai[,,"total"]/infections.averted
    lai.per.IA = apply(initiated.per.infection.averted,c("intervention"),quantile,probs = c(0.025,0.5,0.975),na.rm = T)
    #write.csv(lai.per.IA, file = paste0("cached/lai.per.IA_",Sys.Date(),".csv"))
    
    initiated.per.infection.averted.15.to.24 = initiated.on.lai[,,"total"]/infections.averted.15.to.24
    lai.per.IA.15.to.24 = apply(initiated.per.infection.averted.15.to.24,c("intervention"),quantile,probs = c(0.025,0.5,0.975),na.rm = T)
    #write.csv(lai.per.IA.15.to.24, file = paste0("cached/lai.per.IA.15.to.24_",Sys.Date(),".csv"))    
}

infections.averted.summary = calculate.infections.averted.summary(full.results.array,
                                                                  interventions = INTERVENTIONS,
                                                                  years = YEARS.TO.SUMMARIZE)

infections.averted.15.to.24.summary = calculate.infections.averted.summary(full.results.array,
                                                                           interventions = INTERVENTIONS,
                                                                           ages = c("15-19","20-24"),
                                                                           years = YEARS.TO.SUMMARIZE)

#infections.averted.summary
#infections.averted.15.to.24.summary   

#write.csv(infections.averted.summary, file = paste0("cached/nextgen_infections_averted_",Sys.Date(),".csv"))
#write.csv(infections.averted.15.to.24.summary, file = paste0("cached/nextgen_infections_averted_15-24_",Sys.Date(),".csv"))

