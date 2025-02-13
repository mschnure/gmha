load("mcmc_runs/mcmc_files/mcmc_france_2025-02-13.Rdata")
simset.no.burn = extract.simset(mcmc,
                        additional.burn=1,
                        additional.thin=1) 

load("~/gmha/mcmc_runs/simset_france_2025-02-12.Rdata")
simset.old=simset
params.first.old = simset.old@parameters[1,]

load("~/gmha/mcmc_runs/simset_france_2025-02-13.Rdata")
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

params.manual[transmission.multipliers] = params.default[transmission.multipliers]

sim.manual = run.model.for.parameters(variable.parameters = params.manual,
                                      end.year = 2040,
                                      location = simset@simulations[[1]]$location)

sim.first = simset.no.burn@simulations[[1]]
sim.last = simset.new@simulations[[simset.new@n.sim]]
sim.old.last = simset.old@simulations[[simset.old@n.sim]]

simplot(#sim.france, 
        #sim.first,
        sim.old.last,
        sim.last,
        #sim.manual,
        years=1980:2030, 
        facet.by='age', 
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