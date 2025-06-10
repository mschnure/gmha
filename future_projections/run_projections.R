##############################################
# Description: Code to run future projections
##############################################

source("model/run_systematic.R")
source("future_projections/extract_projection_results.R")

RUN.INDIV.COUNTRY = F
LOAD.GLOBAL.SIMSET = T

# running individual countries through 2040 
if(RUN.INDIV.COUNTRY){
  load("mcmc_runs/mcmc_files/mcmc_non.unaids.remainder_2025-04-25.Rdata")
  
  
  simset = suppressWarnings(extract.simset(mcmc,
                                           additional.burn=7000, # burn half; 7000; 2000 (unaids); 3750 (SA); 5770 (Kenya); 7300 (Tanz)
                                           additional.thin=40)) # thin to 200; 40; 15 (unaids), 33 (SA); 32 (Kenya); 38 (Tanz)
  
  RUN.SIMULATIONS.TO.YEAR = 2040
  print("running no.int")
  simset.no.int = run.intervention.on.simset(simset,
                                             end.year = RUN.SIMULATIONS.TO.YEAR,
                                             intervention = NO.INTERVENTION)
}

# global simset is already through 2040 
if(LOAD.GLOBAL.SIMSET){
  print("loading global simset")
  load("cached/simset_global_2025-04-15.Rdata")
  #load("cached/simset_global_2025-05-01_.Rdata")
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
summary.results$median.over.50.year = pull.year.for.statistic.for.simset(simset=simset.list.full$no.int,
                                                                         data.type = "prevalence",
                                                                         statistic.threshold = 0.5,
                                                                         age.threshold = 50)
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
save(simset.list.full,
     full.results.array,
     summary.results,
     export.to.csv,
     file = paste0("cached/all.results_",convert_string(simset@simulations[[1]]$location),"_", Sys.Date(),".Rdata"))
