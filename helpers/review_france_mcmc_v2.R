load("mcmc_runs/mcmc_files/mcmc_france_2025-02-13.Rdata")
simset.no.burn = extract.simset(mcmc,
                        additional.burn=1,
                        additional.thin=1) 

load("mcmc_runs/simset_france_2025-02-12.Rdata")
simset.old=simset
params.first.old = simset.old@parameters[1,]
params.last.old = simset.old@parameters[simset.old@n.sim,]

load("mcmc_runs/simset_france_2025-02-13.Rdata")
simset.new=simset
params.first.new = simset.new@parameters[1,]

params.manual = params.first.new

params.default = get.default.parameters("France")
cbind(params.default[names(params.first.old)],params.first.old,
      params.first.old/params.default[names(params.first.old)]) 
cbind(params.default[names(params.first.new)],params.first.new,
      params.manual,
      params.first.new/params.default[names(params.first.new)]) 

# params.manual["log.OR.suppression.slope"] = 0
# params.manual["log.OR.testing.intercept"] = 0
# params.manual["log.OR.testing.slope"] = 0
# params.manual["log.OR.engagement.slope"] = 0

transmission.multipliers = names(params.manual[grepl("transmission.multiplier",names(params.manual))])
trates = names(params.manual[grepl("trate",names(params.manual))])

params.last.old[trates]
params.first.old[trates]

params.manual[transmission.multipliers] = params.default[transmission.multipliers]

sim.manual = run.model.for.parameters(variable.parameters = params.manual,
                                      end.year = 2040,
                                      location = simset@simulations[[1]]$location)

sim.first = simset.no.burn@simulations[[1]]
sim.last = simset.new@simulations[[simset.new@n.sim]]
sim.old.last = simset.old@simulations[[simset.old@n.sim]]
sim.old.first = simset.old@simulations[[1]]

simplot(#sim.france, 
        #sim.first,
        sim.old.first,
        #sim.old.last,
        #sim.last,
        #sim.manual,
        years=1980:2030, 
        #facet.by='age', 
        #ages = MODEL.TO.SURVEILLANCE.AGE.MAPPING$`All ages`,
        data.types='incidence')

simplot(sim.old.last,
        sim.last,
        years=1980:2020, 
        data.types='suppression', 
        proportion=T)

source("calibration/likelihood/individual_likelihoods.R") # make sure to change country to France before sourcing 
likelihood.to.run = create.likelihood(parameters = create.model.parameters(location = "France"),
                                      location="France")

exp(likelihood.to.run(sim.last) - likelihood.to.run(sim.old.last))
exp(full.lik(sim.last) - full.lik(sim.old.last))
exp(pop.lik(sim.last) - pop.lik(sim.old.last))
exp(incidence.lik(sim.last) - incidence.lik(sim.old.last))
exp(prev.lik(sim.last) - prev.lik(sim.old.last))
exp(aware.lik(sim.last) - aware.lik(sim.old.last)) 
exp(eng.lik(sim.last) - eng.lik(sim.old.last))
exp(supp.lik(sim.last) - supp.lik(sim.old.last)) 
exp(hiv.mortality.lik(sim.last) - hiv.mortality.lik(sim.old.last))
exp(aware.trend.lik(sim.last) - aware.trend.lik(sim.old.last))
exp(total.mortality.lik(sim.last) - total.mortality.lik(sim.old.last)) 


variable.parameters.france=get.default.parameters(location = "France")

sim.france = run.model.for.parameters(location="France",
                                      variable.parameters = variable.parameters.france,end.year = 2040)

old.params.names = names(params.last.old)
old.params.names = old.params.names[-c(32,33)]
variable.parameters.france[old.params.names] = params.last.old[old.params.names]

# old sim; crazy values for suppression for men - this is what it was using to hit incidence
sim.old.last$parameters$time.varying.parameters$SUPPRESSION.RATES$values[3] # 2003, crazy values (107--> 0.009328123 years to suppress)

# default parameters: more reasonable 
sim.france$parameters$time.varying.parameters$SUPPRESSION.RATES$values[29] # 2003, more reasonable (0.2 --> 4.6 years to suppress) 
sim.france$parameters$time.varying.parameters$SUPPRESSION.RATES$values[46] # 2020, more reasonable (1.5 --> .63 years to suppress) 

# sim.last, with suppression slope OR of 0.181430615
sim.last$parameters$time.varying.parameters$SUPPRESSION.RATES$values[19] # 1993, (0.2286647 --> 4.373215 years to suppress) 
sim.last$parameters$time.varying.parameters$SUPPRESSION.RATES$values[29] # 2003, (3.847767 --> 0.25 years to suppress) 
sim.last$parameters$time.varying.parameters$SUPPRESSION.RATES$values[46] # 2020, (9.647761 --> 0.103651 years to suppress) 


params.last.old["suppression.rate.0"]
params.last.old["suppression.rate.1"]
variable.parameters.france["log.OR.suppression.slope"] = 0.25
variable.parameters.france["male.suppression.multiplier"] = 1

sim.manual = run.model.for.parameters(location="France",
                                      variable.parameters = variable.parameters.france,end.year = 2040)

