##############################################
# Description: Code to run future projections
##############################################

source("model/run_systematic.R")
source("future_projections/extract_projection_results.R")
load("mcmc_runs/mcmc_files/mcmc_mozambique_2025-03-22.Rdata")

simset = suppressWarnings(extract.simset(mcmc,
                                         additional.burn=7000, # burn half
                                         additional.thin=40)) # thin to 200


#load("mcmc_runs/simset_mozambique_2025-03-22.Rdata")

RUN.SIMULATIONS.TO.YEAR = 2040
print("running no.int")
simset.no.int = run.intervention.on.simset(simset,
                                           end.year = RUN.SIMULATIONS.TO.YEAR,
                                           intervention = NO.INTERVENTION)

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

export.to.csv = generate.csv(summary.results)

print("saving all results")
save(simset.list.full,
     full.results.array,
     summary.results,
     export.to.csv,
     file = paste0("cached/all.results_",convert_string(simset@simulations[[1]]$location),"_", Sys.Date(),".Rdata"))
