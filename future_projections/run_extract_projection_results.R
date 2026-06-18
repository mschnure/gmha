##############################################
# Description: Code to run future projections
##############################################

source("model/run_systematic.R")
source("future_projections/extract_projection_results_functions.R")

# only pick one of these
INDIVIDUAL.COUNTRIES = T
GLOBAL = F

# decide whether to load saved simset (temp) or final MCMC (final step)
USE.SIMSET = T 
USE.MCMC = F

DATE.TO.LOAD = "2026-06-05"

N.CHAINS = 1

COUNTRIES = "r1.high"
# COUNTRIES = c("Kenya","Malawi","Mozambique","Nigeria","South Africa","Tanzania","Uganda","Zambia","Zimbabwe",
#               "unaids.remainder","non.unaids.remainder",
#               "r1.low", "r1.lower.middle","r1.upper.middle","r1.high")

for(country in COUNTRIES){
  
  
  if(USE.SIMSET){
    file = paste0("simset_",tolower(country),"_chain1_",DATE.TO.LOAD,".Rdata")
    print(paste0("loading file: ",file))
    load(paste0("mcmc_runs/",file))
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
  if(simset@simulations[[1]]$location %in% c("Uganda","non.unaids.remainder","r1.upper.middle","all.low")){
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