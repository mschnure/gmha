##############################################
# Description: Code to run future projections
##############################################

source("model/run_systematic.R")
source("future_projections/extract_projection_results.R")

RUN.INDIV.COUNTRY = T
LOAD.GLOBAL.SIMSET = F
N.CHAINS = 2

if(LOAD.GLOBAL.SIMSET){
  COUNTRIES = "global"
} else if(RUN.INDIV.COUNTRY){
  COUNTRIES = "South Africa"
  # COUNTRIES = c(#"Mozambique","Uganda","Kenya","Zambia"
  #               #"Zimbabwe","unaids.remainder","non.unaids.remainder"
  #               #"r1.low","r1.lower.middle","r1.upper.middle","r1.high"
  #               #"Tanzania","South Africa",
  #               #"Malawi" #,"Nigeria"
  # 
  #   )
} else stop("can only select run.indiv.country or load.global.simset")

for(country in COUNTRIES){
  
  # running individual countries through 2040
  if(RUN.INDIV.COUNTRY){
    
    if(N.CHAINS==1){
      mcmc.files = list.files("mcmc_runs/mcmc_files")
      if(country=="South Africa") country = "South_Africa"
      mcmc.file = mcmc.files[grepl(paste0(tolower(country),"_2025"),mcmc.files)]
      # if multiple, this will take the last one by date which is the most recent 
      mcmc.file = mcmc.file[length(mcmc.file)]
      
      print(paste0("loading file: ",mcmc.file))
      load(paste0("mcmc_runs/mcmc_files/",mcmc.file))
      
      # If running from single chain, burn 10000 and thin by 20 to get to 1000 
      simset = suppressWarnings(extract.simset(mcmc,
                                               additional.burn=10000, 
                                               additional.thin=10)) 
    } else if(N.CHAINS==2){
      mcmc.files = list.files("mcmc_runs/mcmc_files/merged")
      if(country=="South Africa") country = "South_Africa"
      mcmc.file = mcmc.files[grepl(paste0("chains12_",tolower(country),"_2025"),mcmc.files)]
      # if multiple, this will take the last one by date which is the most recent 
      mcmc.file = mcmc.file[length(mcmc.file)]
      
      print(paste0("loading file: ",mcmc.file))
      load(paste0("mcmc_runs/mcmc_files/merged/",mcmc.file))
      
      # If running from two chains, burn X and thin by Y to get to Z (without anything, XX sims)
      simset = suppressWarnings(extract.simset(mcmc,
                                               additional.burn=10000, 
                                               additional.thin=20)) 
      save(simset,file=paste0("mcmc_runs/simset_",convert_string(simset@simulations[[1]]$location),"_chains12_",Sys.Date(),".Rdata")) 
      
      
    } else
      stop("N.CHAINS must be either 1 or 2")

    
    RUN.SIMULATIONS.TO.YEAR = 2040
    print("running no.int")
    simset.no.int = run.intervention.on.simset(simset,
                                               end.year = RUN.SIMULATIONS.TO.YEAR,
                                               intervention = NO.INTERVENTION)
  }
  
  
  # global simset is already through 2040 
  if(LOAD.GLOBAL.SIMSET){
    print("loading global simset")
    #load("cached/simset_global_income_2025-08-26_.Rdata") # using income models 
    load("cached/simset_global_2025-08-27_.Rdata") # using remainder model
    simset.no.int = simset
  }
  
  simset.list.full = list(no.int = simset.no.int)
  
  print("generating full.results.array")
  full.results.array = generate.full.results.array(simset.list = simset.list.full)
  
  ## BOTH SEXES ## 
  print("generating summary statistics, both sexes")
  summary.results = list()
  summary.results$prevalence.incidence.median.age.table = generate.median.age.table(simset.list = simset.list.full,
                                                                                    data.types = c("prevalence","incidence"),
                                                                                    years = c(2025,2040))
  summary.results$prevalence.percent.over.50.table = generate.percent.over.age.table(simset.list = simset.list.full,
                                                                                     age.point=50,
                                                                                     data.types = c("prevalence"),
                                                                                     years=c(2025,2040))
  summary.results$prevalence.percent.over.65.table = generate.percent.over.age.table(simset.list = simset.list.full,
                                                                                     age.point=65,
                                                                                     data.types = c("prevalence"),
                                                                                     years=c(2025,2040))
  summary.results$prevalence.number.over.50.table = generate.number.over.age.table(simset.list = simset.list.full,
                                                                                   age.point=50,
                                                                                   data.types = c("prevalence"),
                                                                                   years=c(2025,2040))
  summary.results$prevalence.number.over.65.table = generate.number.over.age.table(simset.list = simset.list.full,
                                                                                   age.point=65,
                                                                                   data.types = c("prevalence"),
                                                                                   years=c(2025,2040))
  if(simset@simulations[[1]]$location %in% c("Uganda","non.unaids.remainder","r1.upper.middle")){
    summary.results$median.over.50.year = rep(NA,3)
  } else {
    summary.results$median.over.50.year = pull.year.for.statistic.for.simset(simset=simset.list.full$no.int,
                                                                             data.type = "prevalence",
                                                                             statistic.threshold = 0.5,
                                                                             age.threshold = 50)
  }
  
  # summary.results$quarter.over.65.year = pull.year.for.statistic.for.simset(simset=simset.list.full$no.int,
  #                                                          data.type = "prevalence",
  #                                                          statistic.threshold = 0.25,
  #                                                          age.threshold = 65)
  summary.results$quarter.over.65.year = rep(NA,3)
  summary.results$prevalence.age.distribution = generate.age.distribution.2.column(full.results.array,
                                                                                   outcome="prevalence",
                                                                                   intervention.1="no.int",year.1="2025",
                                                                                   intervention.2="no.int",year.2="2040",
                                                                                   percent=F,display="table")
  
  prev.2025 = apply(full.results.array["2025",,,"prevalence",,], "sim",sum)
  summary.results$prev.2025 = quantile(prev.2025,probs=c(.025,.5,.975),na.rm=T)
  
  prev.2040 = apply(full.results.array["2040",,,"prevalence",,], "sim",sum)
  summary.results$prev.2040 = quantile(prev.2040,probs=c(.025,.5,.975),na.rm=T)
  
  export.to.csv = generate.csv(summary.results)
  
  print("saving all results")
  if(N.CHAINS==2){
    save(simset.list.full,
         full.results.array,
         summary.results,
         export.to.csv,
         file = paste0("cached/all.results_merged_",convert_string(simset@simulations[[1]]$location),"_", Sys.Date(),".Rdata"))
  } else {
    save(simset.list.full,
         full.results.array,
         summary.results,
         export.to.csv,
         file = paste0("cached/all.results_",convert_string(simset@simulations[[1]]$location),"_", Sys.Date(),".Rdata"))
    
    # save(simset.list.full,
    #      full.results.array,
    #      summary.results,
    #      export.to.csv,
    #      file = paste0("cached/all.results_",convert_string(simset@simulations[[1]]$location),"_income_", Sys.Date(),".Rdata"))
    
  }

  
}