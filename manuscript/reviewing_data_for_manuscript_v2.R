source("model/run_systematic.R")

load("~/gmha/mcmc_runs/simset_r1.high_2026-04-13.Rdata")
simset.old = simset

load("~/gmha/mcmc_runs/simset_r1.high_1_2026-04-20.Rdata")
simset.bad = simset

load("~/gmha/mcmc_runs/simset_r1.high_1_2026-04-23.Rdata")
simset.new = simset

simplot(simset.old,
        #simset.bad,
        simset.new, 
        years=1980:2040, 
        facet.by='age', 
        ages = MODEL.TO.SURVEILLANCE.AGE.MAPPING$`All ages`,
        data.types='incidence')+ geom_vline(xintercept = 1997)+ geom_vline(xintercept = 2018)

# time.0=1990
# time.1=1997 
# time.2=2008 
# time.3=2018 

params.old = simset.old@parameters[490,] 
params.bad = simset.bad@parameters[490,] 
params.new = simset.new@parameters[490,] 

cbind(params.old[grepl("transmission.multiplier|trate",names(params.old))],
      params.bad[grepl("transmission.multiplier|trate",names(params.bad))],
      params.new[grepl("transmission.multiplier|trate",names(params.new))])
