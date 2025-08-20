##############################################
# Description: Code to run future projections
##############################################

source("model/run_systematic.R")
source("future_projections/extract_projection_results.R")

RUN.INDIV.COUNTRY = T
LOAD.GLOBAL.SIMSET = F

COUNTRIES = c("Mozambique","Uganda","Kenya","Zambia","Zimbabwe","unaids.remainder","non.unaids.remainder","r1.low",
              "r1.lower.middle","r1.upper.middle","r1.high")

if(RUN.INDIV.COUNTRY){
    for(country in COUNTRIES){
        mcmc.files = list.files("mcmc_runs/mcmc_files")
        mcmc.file = mcmc.files[grepl(tolower(country),mcmc.files)]
        # running individual countries through 2040 
        
        load(paste0("mcmc_runs/mcmc_files/",mcmc.file))
        
        # if running from two previous runs (100k at 1/8 weight; 50k at 4x prevalence weight, don't burn anything 
        # if running from single chain, burn half and thin by X to get to 1,000 
        # if running from two chains, burn half and thin by Y to get to 1,000
        simset = suppressWarnings(extract.simset(mcmc,
                                                 additional.burn=1, # 
                                                 additional.thin=50)) # thin by 50 to to 200
        
        RUN.SIMULATIONS.TO.YEAR = 2040
        print("running no.int")
        simset.no.int = run.intervention.on.simset(simset,
                                                   end.year = RUN.SIMULATIONS.TO.YEAR,
                                                   intervention = NO.INTERVENTION)
    }
    
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
if(simset@simulations[[1]]$location=="Uganda"){
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
save(simset.list.full,
     full.results.array,
     summary.results,
     export.to.csv,
     file = paste0("cached/all.results_",convert_string(simset@simulations[[1]]$location),"_", Sys.Date(),".Rdata"))
