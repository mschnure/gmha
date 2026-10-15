source("model/run_systematic.R")

load("~/gmha/mcmc_runs/simset_nigeria_chain1_2026-08-21.Rdata")
simset.step1 = simset
load("~/gmha/mcmc_runs/simset_nigeria_chain1_2026-09-26.Rdata")
simset.chain1 = simset
# load("~/gmha/cached/all.results_nigeria_2025-08-25.Rdata")
# simset.old = simset.list.full$no.int

LOCATION = "Nigeria"

# old data manager
#load('cached/data.manager_global_2026-06-25.Rdata') # added back median values for suppressed data

simplot(simset.step1@simulations[[1]],
        simset.step1@simulations[[length(simset.step1@simulations)]],
        #simset.chain1,
        years=1980:2040, 
        facet.by='age', 
        #ages = MODEL.TO.SURVEILLANCE.AGE.MAPPING$`All ages`,
        data.types='incidence')

if(1==2){
  source("~/gmha/calibration/likelihood/individual_likelihoods.R")
  
  WEIGHTED.PREVALENCE = F 
  TOTAL.MORTALITY.WEIGHT = 1/1000 # SET BACK TO 1/1000 (FROM 1/100)
  N.ITER = 100000
  TOTAL.WEIGHT = 1
  
  WEIGHT.YEARS = 1970:2030
  WEIGHTS.BY.YEAR = rep(1, length(WEIGHT.YEARS))
  WEIGHTS.BY.YEAR = (1/4)^(WEIGHT.YEARS<2000) # before 2000, 1/4x
  WEIGHTS.BY.YEAR[WEIGHT.YEARS>=2018] = 4 # from 2018, 4x
  names(WEIGHTS.BY.YEAR) = WEIGHT.YEARS
  WEIGHTS.BY.YEAR = WEIGHTS.BY.YEAR*TOTAL.WEIGHT
  POP.WEIGHTS = c(WEIGHTS.BY.YEAR,rep(TOTAL.WEIGHT,10))
  names(POP.WEIGHTS) = c(names(WEIGHTS.BY.YEAR),2031:2040)
  
  LOCATION.DETAILS = set.likelihood.and.prior.by.location(location=LOCATION,
                                                          total.weight = TOTAL.WEIGHT,
                                                          weighted.prevalence = WEIGHTED.PREVALENCE,
                                                          total.mortality.weight = TOTAL.MORTALITY.WEIGHT)
  
  exp(incidence.lik(simset.chain1@simulations[[simset.chain1@n.sim]]) - 
        incidence.lik(simset.step1@simulations[[simset.step1@n.sim]]))
  
  exp(prev.lik(simset.chain1@simulations[[simset.chain1@n.sim]]) - 
        prev.lik(simset.step1@simulations[[simset.step1@n.sim]]))
  
  exp(full.lik(simset.chain1@simulations[[simset.chain1@n.sim]]) - 
        full.lik(simset.step1@simulations[[simset.step1@n.sim]]))
  
  exp(LOCATION.DETAILS$likelihood.to.run(simset.chain1@simulations[[simset.chain1@n.sim]]) - 
        LOCATION.DETAILS$likelihood.to.run(simset.step1@simulations[[simset.step1@n.sim]]))
  
}

round(cbind(simset.step1@parameters[1,],
      simset.step1@parameters[length(simset.step1@simulations),]),4)

sim.step1 = simset.step1@simulations[[simset.step1@n.sim]]
params.step1 = simset.step1@parameters[simset.step1@n.sim,]

sim.step1 = run.model.for.parameters(variable.parameters = params.step1,
                                    end.year = 2040,
                                    location = sim.step1$location)

params.manual = params.step1
round(cbind(params.manual),4)

sim.manual = run.model.for.parameters(variable.parameters = params.manual,
                                      end.year = 2040,
                                      location = sim.step1$location)

params.manual["age.50.and.over.transmission.multiplier.3"] #

params.manual["age.50.and.over.transmission.multiplier.3"] #= 0.25 # 0.3415277 

sim.manual = run.model.for.parameters(variable.parameters = params.manual,
                                      end.year = 2040,
                                      location = sim.step1$location)

simplot(simset.step1@simulations[[1]],
        #sim.manual,
        #simset.chain1,
        years=1980:2040, 
        #facet.by='age', 
        #ages = MODEL.TO.SURVEILLANCE.AGE.MAPPING$`All ages`,
        data.types='incidence')

simplot(sim.step1,
        sim.manual,
        #simset.chain1,
        years=1980:2040, 
        facet.by='age', 
        #ages = MODEL.TO.SURVEILLANCE.AGE.MAPPING$`All ages`,
        data.types='incidence')


