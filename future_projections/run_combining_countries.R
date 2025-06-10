source('model/run_systematic.R')
source("future_projections/combining_countries.R")

load("cached/combined.countries.Rdata") # combined object; ran the below lines and then saved it 

combined.countries.sorted = sort.by.prevalence(combined.countries)

# collapse over the country dimension of the full array 
collapsed.list = collapse.country.dim(combined.countries.sorted)

# convert into a list (not quite the simset, just the simulations list)
simulation.list = convert.to.simset(collapsed.list)

# check to make sure it worked 
#table(collapsed.list$population[,,,,,35]==simulation.list[[35]]$population[,,,,]) # both of these will collapse over the subgroup dimension but it's there 
#table(collapsed.list$disengagement.unsuppressed[,,,,65]==simulation.list[[65]]$disengagement.unsuppressed[,,,])

# load up a simset (e.g., Malawi), replace its list of simulations with the global simulation.list we made above
# now, we have a global simset
load("cached/all.results_malawi_2025-04-10.Rdata")
simset = simset.list.full$no.int
simset@simulations = simulation.list

save(simset,file="cached/simset_global_2025-04-15.Rdata")

# this will take ~45 min; do it once and then save/load the file later 
if(1==2){
    load("cached/all.results_unaids.remainder_2025-04-10.Rdata")
    simset.unaids = simset.list.full$no.int
    
    load("cached/all.results_non.unaids.remainder_2025-04-10.Rdata")
    simset.non.unaids = simset.list.full$no.int
    
    load("cached/all.results_south_africa_2025-04-11.Rdata") 
    simset.south.africa = simset.list.full$no.int
    
    load("cached/all.results_mozambique_2025-04-10.Rdata")
    simset.mozambique = simset.list.full$no.int
    
    load("cached/all.results_nigeria_2025-04-10.Rdata")
    simset.nigeria = simset.list.full$no.int
    
    load("cached/all.results_tanzania_2025-04-11.Rdata")
    simset.tanzania = simset.list.full$no.int
    
    load("cached/all.results_uganda_2025-04-10.Rdata")
    simset.uganda = simset.list.full$no.int
    
    load("cached/all.results_kenya_2025-04-11.Rdata")
    simset.kenya = simset.list.full$no.int
    
    load("cached/all.results_zambia_2025-04-10.Rdata")
    simset.zambia = simset.list.full$no.int
    
    load("cached/all.results_zimbabwe_2025-04-10.Rdata")
    simset.zimbabwe = simset.list.full$no.int
    
    load("cached/all.results_malawi_2025-04-10.Rdata")
    simset.malawi = simset.list.full$no.int
    
    combined.countries = combine.simsets(simset.unaids,
                                         simset.non.unaids,
                                         simset.south.africa,
                                         simset.mozambique,
                                         simset.nigeria,
                                         simset.tanzania,
                                         simset.uganda,
                                         simset.kenya,
                                         simset.zambia,
                                         simset.zimbabwe,
                                         simset.malawi,
                                         countries = c("unaids.remainder","non.unaids.remainder","south.africa","mozambique",
                                                       "nigeria","tanzania","uganda","kenya","zambia","zimbabwe","malawi"))
    
    # make sure they're all the same 
    table(combined.countries$population[,,,,,1,1]==simset.unaids@simulations[[1]]$population[,,,1,])
    table(combined.countries$non.hiv.mortality[,,,,,35,1]==simset.unaids@simulations[[35]]$non.hiv.mortality[,,,1,])

    table(combined.countries$population[,,,,,1,2]==simset.non.unaids@simulations[[1]]$population[,,,1,])
    table(combined.countries$engagement[,,,,45,2]==simset.non.unaids@simulations[[45]]$engagement[,,,1])

    table(combined.countries$population[,,,,,1,7]==simset.uganda@simulations[[1]]$population[,,,1,])
    table(combined.countries$prevalence[,,,,,65,7]==simset.uganda@simulations[[65]]$prevalence[,,,1,])
    
    save(combined.countries,file="cached/combined.countries.Rdata")
    
}