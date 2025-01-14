#####################################################
# Description: Code to run interventions on simset.29 
#####################################################

source("model/run_systematic.R")
source("future_projections/extract_projection_results.R")
load("~/Dropbox/Documents_local/Hopkins/SOM_Job/3_Aging_multimorbidity/gmha/mcmc_runs/mcmc_files/mcmc_kenya_2025-01-13.Rdata")

simset = suppressWarnings(extract.simset(mcmc,
                                         additional.burn=500,
                                         additional.thin=20))

# simset = suppressWarnings(extract.simset(mcmc,
#                                          additional.burn=5998,
#                                          additional.thin=1))

#load("mcmc_runs/simset_kenya_2025-01-13.Rdata")
#load("mcmc_runs/simset_south_africa_2025-01-13.Rdata")

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
prevalence.engagement.median.age.table = generate.median.age.table(simset.list = simset.list.full,
                                                                   data.types = c("prevalence","engagement"),
                                                                   years = c(2025,2040))
incidence.annual.engagement.median.age.table = generate.median.age.table(simset.list = simset.list.full,
                                                                         data.types = c("incidence","annual.engagement"),
                                                                         years = c(2025,2040))
prevalence.engagement.over.50.table = generate.percent.over.age.table(simset.list = simset.list.full,
                                                                      age.point=50,
                                                                      data.types = c("prevalence","engagement"),
                                                                      years=c(2025,2040))
incidence.over.30.table = generate.percent.over.age.table(simset.list = simset.list.full,
                                                          age.point=30,
                                                          data.types = c("incidence"),
                                                          years=c(2025,2040))
annual.engagement.over.30.table = generate.percent.over.age.table(simset.list = simset.list.full,
                                                                  age.point=30,
                                                                  data.types = c("annual.engagement"),
                                                                  years=c(2025,2040))

## FEMALE ONLY ## 
print("generating summary statistics, female")
prevalence.engagement.median.age.table.female = generate.median.age.table(simset.list = simset.list.full,
                                                                          data.types = c("prevalence","engagement"),
                                                                          years = c(2025,2040),
                                                                          sexes = "female")
incidence.annual.engagement.median.age.table.female = generate.median.age.table(simset.list = simset.list.full,
                                                                                data.types = c("incidence","annual.engagement"),
                                                                                years = c(2025,2040),
                                                                                sexes = "female")
prevalence.engagement.over.50.table.female = generate.percent.over.age.table(simset.list = simset.list.full,
                                                                             age.point=50,
                                                                             data.types = c("prevalence","engagement"),
                                                                             years=c(2025,2040),
                                                                             sexes = "female")
incidence.over.30.table.female = generate.percent.over.age.table(simset.list = simset.list.full,
                                                                 age.point=30,
                                                                 data.types = c("incidence"),
                                                                 years=c(2025,2040),
                                                                 sexes = "female")
annual.engagement.over.30.table.female = generate.percent.over.age.table(simset.list = simset.list.full,
                                                                         age.point=30,
                                                                         data.types = c("annual.engagement"),
                                                                         years=c(2025,2040),
                                                                         sexes = "female")

## MALE ONLY ## 
print("generating summary statistics, male")
prevalence.engagement.median.age.table.male = generate.median.age.table(simset.list = simset.list.full,
                                                                        data.types = c("prevalence","engagement"),
                                                                        years = c(2025,2040),
                                                                        sexes = "male")
incidence.annual.engagement.median.age.table.male = generate.median.age.table(simset.list = simset.list.full,
                                                                              data.types = c("incidence","annual.engagement"),
                                                                              years = c(2025,2040),
                                                                              sexes = "male")
prevalence.engagement.over.50.table.male = generate.percent.over.age.table(simset.list = simset.list.full,
                                                                           age.point=50,
                                                                           data.types = c("prevalence","engagement"),
                                                                           years=c(2025,2040),
                                                                           sexes = "male")
incidence.over.30.table.male = generate.percent.over.age.table(simset.list = simset.list.full,
                                                               age.point=30,
                                                               data.types = c("incidence"),
                                                               years=c(2025,2040),
                                                               sexes = "male")
annual.engagement.over.30.table.male = generate.percent.over.age.table(simset.list = simset.list.full,
                                                                       age.point=30,
                                                                       data.types = c("annual.engagement"),
                                                                       years=c(2025,2040),
                                                                       sexes = "male")
print("saving all results")
save(simset.list.full,
     full.results.array,
     prevalence.engagement.median.age.table,
     incidence.annual.engagement.median.age.table,
     prevalence.engagement.over.50.table,
     incidence.over.30.table,
     annual.engagement.over.30.table,
     
     prevalence.engagement.median.age.table.female,
     incidence.annual.engagement.median.age.table.female,
     prevalence.engagement.over.50.table.female,
     incidence.over.30.table.female,
     annual.engagement.over.30.table.female,
     
     prevalence.engagement.median.age.table.male,
     incidence.annual.engagement.median.age.table.male,
     prevalence.engagement.over.50.table.male,
     incidence.over.30.table.male,
     annual.engagement.over.30.table.male,
     
     file = paste0("cached/all.results_",convert_string(simset@simulations[[1]]$location),"_", Sys.Date(),".Rdata"))
