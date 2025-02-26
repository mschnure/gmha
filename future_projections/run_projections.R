##############################################
# Description: Code to run future projections
##############################################

source("model/run_systematic.R")
source("future_projections/extract_projection_results.R")
#load("mcmc_runs/mcmc_files/mcmc_kenya_2025-01-13.Rdata")
#load("mcmc_runs/mcmc_files/mcmc_kenya_2025-01-16.Rdata")

# simset = suppressWarnings(extract.simset(mcmc,
#                                          additional.burn=500,
#                                          additional.thin=20))

# simset = suppressWarnings(extract.simset(mcmc,
#                                          additional.burn=5998,
#                                          additional.thin=1))


#load("mcmc_runs/simset_kenya_thinned_2025-02-11.Rdata")
#load("mcmc_runs/simset_south_africa_thinned_2025-02-11.Rdata")
#load("mcmc_runs/simset_france_thinned_2025-02-12.Rdata")

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
prevalence.incidence.median.age.table = generate.median.age.table(simset.list = simset.list.full,
                                                                   data.types = c("prevalence","incidence"),
                                                                   years = c(2025,2040))
prevalence.incidence.over.50.table = generate.percent.over.age.table(simset.list = simset.list.full,
                                                                      age.point=50,
                                                                      data.types = c("prevalence","incidence"),
                                                                      years=c(2025,2040))
prevalence.incidence.over.65.table = generate.percent.over.age.table(simset.list = simset.list.full,
                                                                     age.point=65,
                                                                     data.types = c("prevalence","incidence"),
                                                                     years=c(2025,2040))
median.over.50.year = pull.year.for.statistic.for.simset(simset=simset.list.full$no.int,
                                                         data.type = "prevalence",
                                                         statistic.threshold = 0.5,
                                                         age.threshold = 50)
quarter.over.65.year = pull.year.for.statistic.for.simset(simset=simset.list.full$no.int,
                                                         data.type = "prevalence",
                                                         statistic.threshold = 0.25,
                                                         age.threshold = 65)


## FEMALE ONLY ## 
print("generating summary statistics, female")
prevalence.incidence.median.age.table.female = generate.median.age.table(simset.list = simset.list.full,
                                                                         data.types = c("prevalence","incidence"),
                                                                         years = c(2025,2040),
                                                                         sexes = "female")
prevalence.incidence.over.50.table.female = generate.percent.over.age.table(simset.list = simset.list.full,
                                                                            age.point=50,
                                                                            data.types = c("prevalence","incidence"),
                                                                            years=c(2025,2040),
                                                                            sexes = "female")
prevalence.incidence.over.65.table.female = generate.percent.over.age.table(simset.list = simset.list.full,
                                                                            age.point=65,
                                                                            data.types = c("prevalence","incidence"),
                                                                            years=c(2025,2040),
                                                                            sexes = "female")

## MALE ONLY ## 
print("generating summary statistics, male")
prevalence.incidence.median.age.table.male = generate.median.age.table(simset.list = simset.list.full,
                                                                       data.types = c("prevalence","incidence"),
                                                                       years = c(2025,2040),
                                                                       sexes = "male")
prevalence.incidence.over.50.table.male = generate.percent.over.age.table(simset.list = simset.list.full,
                                                                          age.point=50,
                                                                          data.types = c("prevalence","incidence"),
                                                                          years=c(2025,2040),
                                                                          sexes = "male")
prevalence.incidence.over.65.table.male = generate.percent.over.age.table(simset.list = simset.list.full,
                                                                          age.point=65,
                                                                          data.types = c("prevalence","incidence"),
                                                                          years=c(2025,2040),
                                                                          sexes = "male")


print("saving all results")
save(simset.list.full,
     full.results.array,
     prevalence.incidence.median.age.table,
     prevalence.incidence.over.50.table,
     prevalence.incidence.over.65.table,
     median.over.50.year,
     quarter.over.65.year,
     
     prevalence.incidence.median.age.table.female,
     prevalence.incidence.over.50.table.female,
     prevalence.incidence.over.65.table.female,
     
     prevalence.incidence.median.age.table.male,
     prevalence.incidence.over.50.table.male,
     prevalence.incidence.over.65.table.male,
     
     file = paste0("cached/all.results_",convert_string(simset@simulations[[1]]$location),"_", Sys.Date(),".Rdata"))
