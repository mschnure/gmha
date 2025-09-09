source('model/run_systematic.R')
source("future_projections/combining_countries.R")

#USE.INCOME.MODELS = F

# combined object; ran the below lines and then saved it 
if(1==1){

  print("loading combined object")
  # load("cached/combined.countries.income_2025-08-26_.Rdata")  # using income models, not unaids.remainder 
  # combined.countries = combined.countries.income
  
  #load("cached/combined.countries_2025-08-26_.Rdata")  # using unaids.remainder 
  
  # income models
  if(1==1){
    load("cached/all.income.models_2025-08-26.Rdata")
    # need to change save line below as well
    combined.countries = all.high.income # all.low.income, all.lower.middle.income, 
                                        # all.upper.middle.income, all.high.income
    
  }
  
  print("sorting by prevalence")
  combined.countries.sorted = sort.by.prevalence(combined.countries)
  
  # collapse over the country dimension of the full array 
  print("collapsing country dimension")
  collapsed.list = collapse.country.dim(combined.countries.sorted)
  
  # convert into a list (not quite the simset, just the simulations list)
  print("converting to simulation list, MAKE SURE TO SET LOCATION")
  simulation.list = convert.to.simset(collapsed.list,location="all.high") # CHANGE THIS LOCATION FOR INCOME MODELS 
  
  # check to make sure it worked 
  #table(collapsed.list$population[,,,,,35]==simulation.list[[35]]$population[,,,,]) # both of these will collapse over the subgroup dimension but it's there 
  #table(collapsed.list$disengagement.unsuppressed[,,,,65]==simulation.list[[65]]$disengagement.unsuppressed[,,,])
  
  print("loading mozambique simset and replacing with global simset list")
  load("cached/all.results_merged_mozambique_2025-08-25.Rdata")
  simset = simset.list.full$no.int
  simset@simulations = simulation.list
  
  #save(simset,file=paste0("cached/simset_global_income_",Sys.Date(),"_.Rdata"))
  #save(simset,file=paste0("cached/simset_global_",Sys.Date(),"_.Rdata"))
  #save(simset,file=paste0("cached/simset_low_income_",Sys.Date(),".Rdata"))
  # save(simset,file=paste0("cached/simset_lower_middle_income_",Sys.Date(),".Rdata"))
  #save(simset,file=paste0("cached/simset_upper_middle_income_",Sys.Date(),".Rdata"))
   save(simset,file=paste0("cached/simset_high_income_",Sys.Date(),".Rdata"))
}

# this will take 5-9 hours; do it once and then save/load the file later 
if(1==2){

  print("loading r1.low, merged")
  load("cached/all.results_merged_r1.low_2025-08-25.Rdata")
  simset.r1.low = simset.list.full$no.int
  
  print("loading r1.lower.middle, merged")
  load("cached/all.results_merged_r1.lower.middle_2025-08-25.Rdata")
  simset.r1.lower.middle = simset.list.full$no.int
  
  print("loading r1.upper.middle, merged")
  load("cached/all.results_merged_r1.upper.middle_2025-08-25.Rdata")
  simset.r1.upper.middle = simset.list.full$no.int
  
  print("loading r1.high, merged")
  load("cached/all.results_merged_r1.high_2025-08-25.Rdata")
  simset.r1.high = simset.list.full$no.int
  
  print("loading unaids.remainder, merged")
  load("cached/all.results_merged_unaids.remainder_2025-08-25.Rdata")
  simset.unaids = simset.list.full$no.int

  print("loading non.unaids.remainder, merged")
  load("cached/all.results_merged_non.unaids.remainder_2025-08-25.Rdata")
  simset.non.unaids = simset.list.full$no.int
  
  print("loading south africa, NOT MERGED (Chain 1 only)")
  load("cached/all.results_south_africa_2025-08-25.Rdata") 
  simset.south.africa = simset.list.full$no.int
  
  print("loading mozambique, merged")
  load("cached/all.results_merged_mozambique_2025-08-25.Rdata")
  simset.mozambique = simset.list.full$no.int
  
  print("loading nigeria, NOT MERGED (Chain 1 only)")
  load("cached/all.results_nigeria_2025-08-25.Rdata")
  simset.nigeria = simset.list.full$no.int
  
  print("loading tanzania, NOT MERGED (Chain 1 only)")
  load("cached/all.results_tanzania_2025-08-25.Rdata")
  simset.tanzania = simset.list.full$no.int
  
  print("loading uganda, merged")
  load("cached/all.results_merged_uganda_2025-08-25.Rdata")
  simset.uganda = simset.list.full$no.int
  
  print("loading kenya, merged")
  load("cached/all.results_merged_kenya_2025-08-25.Rdata")
  simset.kenya = simset.list.full$no.int
  
  print("loading zambia, merged")
  load("cached/all.results_merged_zambia_2025-08-25.Rdata")
  simset.zambia = simset.list.full$no.int
  
  print("loading zimbabwe, merged")
  load("cached/all.results_merged_zimbabwe_2025-08-25.Rdata")
  simset.zimbabwe = simset.list.full$no.int
  
  print("loading malawi, merged")
  load("cached/all.results_merged_malawi_2025-08-25.Rdata")
  simset.malawi = simset.list.full$no.int
  
  pring("loading France, ONLY to merge into all.high")


  #if(USE.INCOME.MODELS){
 # print("combining countries, using income models")
  # combined.countries.income = combine.simsets(simset.r1.low,
  #                                             simset.r1.lower.middle,
  #                                             simset.r1.upper.middle,
  #                                             simset.r1.high,
  #                                             simset.non.unaids,
  #                                             simset.south.africa,
  #                                             simset.mozambique,
  #                                             simset.nigeria,
  #                                             simset.tanzania,
  #                                             simset.uganda,
  #                                             simset.kenya,
  #                                             simset.zambia,
  #                                             simset.zimbabwe,
  #                                             simset.malawi,
  #                                             countries = c("r1.low","r1.lower.middle","r1.upper.middle","r1.high",
  #                                                           "non.unaids.remainder","south.africa","mozambique",
  #                                                           "nigeria","tanzania","uganda","kenya","zambia","zimbabwe","malawi")) 
  #  } else {
  print("combining countries, NOT using income models")
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
  #  }
  
  print("combining all low income models (r1.low, mozambique, uganda, malawi)")
  all.low.income = combine.simsets(simset.r1.low,
                                   simset.mozambique,
                                   simset.uganda,
                                   simset.malawi,
                                   countries = c("r1.low","mozambique",
                                                 "uganda","malawi")) 
  
  print("combining all lower middle income models (r1.lower.middle, kenya, nigeria, tanzania, zambia, zimbabwe)")
  all.lower.middle.income = combine.simsets(simset.r1.lower.middle,
                                            simset.kenya,
                                            simset.nigeria,
                                            simset.tanzania,
                                            simset.zambia,
                                            simset.zimbabwe,
                                            countries = c("r1.lower.middle","kenya","nigeria",
                                                          "tanzania","zambia","zimbabwe")) 
  
  print("combining all upper middle income models (r1.upper middle, south africa)")
  all.upper.middle.income = combine.simsets(simset.r1.upper.middle,
                                            simset.south.africa,
                                            countries = c("r1.upper.middle","south.africa")) 
  
  print("assigning high income model, r1.high only")
  all.high.income = simset.r1.high

  
  # make sure they're all the same 
  table(combined.countries$population[,,,,,1,1]==simset.unaids@simulations[[1]]$population[,,,1,])
  table(combined.countries$non.hiv.mortality[,,,,,35,1]==simset.unaids@simulations[[35]]$non.hiv.mortality[,,,1,])
  
  table(combined.countries$population[,,,,,1,2]==simset.non.unaids@simulations[[1]]$population[,,,1,])
  table(combined.countries$engagement[,,,,45,2]==simset.non.unaids@simulations[[45]]$engagement[,,,1])
  
  table(combined.countries$population[,,,,,1,7]==simset.uganda@simulations[[1]]$population[,,,1,])
  table(combined.countries$prevalence[,,,,,65,7]==simset.uganda@simulations[[65]]$prevalence[,,,1,])
  
  print("saving combined.countries")
  #  if(USE.INCOME.MODELS){
  #save(combined.countries.income,file=paste0("cached/combined.countries.income_",Sys.Date(),"_.Rdata"))  
  #  } else{
  save(combined.countries,file=paste0("cached/combined.countries_",Sys.Date(),"_.Rdata"))  
  #  }
  
  print("saving income models")
  save(all.low.income,
       all.lower.middle.income,
       all.upper.middle.income,
       all.high.income,
       file = paste0("cached/all.income.models_", Sys.Date(),".Rdata"))
  
}
