source("model/run_systematic.R")
source("calibration/likelihood/individual_likelihoods.R") # make sure to change country to France before sourcing 
likelihood.to.run = create.likelihood(parameters = create.model.parameters(location = "France"),
                                      location="France")

load("mcmc_runs/simset_france_2025-02-10.Rdata")
simset.new = simset
sim.mcmc = simset.new@simulations[[simset.new@n.sim]]

sim.first = simset.new@simulations[[1]]
params.mcmc = simset.new@parameters[simset.new@n.sim,]
params.manual = params.mcmc

sim.manual = sim.mcmc
cbind(params.manual)
cbind(params.mcmc)

params.manual["age.15.to.19.aging.factor"] = 2 #151.602202747 # why was this dragged up? --> incidence

sim.manual = run.model.for.parameters(variable.parameters = params.manual,
                                      end.year = 2030,
                                      location = sim.manual$location)
simplot(sim.france, # run from priors 
        sim.mcmc, 
        #sim.manual,
        years = 1980:2030, 
        data.types = "population",
        facet.by='age')

simplot(#sim.france.old, # run from priors 
        sim.france.new,
        #sim.mcmc, 
        #sim.manual,
        years=1980:2030, 
        facet.by=c('age'), 
        #sexes = "male",
        data.types='total.mortality')

simplot(#sim.france.old, # run from priors 
        sim.france.new,
        #sim.mcmc, 
        #sim.manual,
        years=1980:2030, 
        facet.by=c('age'), 
        #ages = MODEL.TO.SURVEILLANCE.AGE.MAPPING$`All ages`,
        data.types='hiv.mortality')

exp(likelihood.to.run(sim.manual) - likelihood.to.run(sim.mcmc))
exp(full.lik(sim.manual) - full.lik(sim.mcmc))
exp(pop.lik(sim.manual) - pop.lik(sim.mcmc))
exp(incidence.lik(sim.manual) - incidence.lik(sim.mcmc)) 
exp(prev.lik(sim.manual) - prev.lik(sim.mcmc))
exp(aware.lik(sim.manual) - aware.lik(sim.mcmc))
exp(eng.lik(sim.manual) - eng.lik(sim.mcmc))
exp(supp.lik(sim.manual) - supp.lik(sim.mcmc)) 
exp(hiv.mortality.lik(sim.manual) - hiv.mortality.lik(sim.mcmc))
exp(aware.trend.lik(sim.manual) - aware.trend.lik(sim.mcmc))
exp(total.mortality.lik(sim.manual) - total.mortality.lik(sim.mcmc)) 

simplot(sim.mcmc, 
        sim.manual,
        years = 1980:2030)

simplot(simset.new, 
        sim.new.last,
        years = 1980:2030, 
        data.types = "population")

simplot(sim.mcmc, 
        sim.manual,
        years=1980:2030, 
        facet.by='age', 
        ages = MODEL.TO.SURVEILLANCE.AGE.MAPPING$`All ages`,
        data.types='incidence')

simplot(simset.new, 
        years=1980:2030, 
        facet.by='age', 
        #ages = MODEL.TO.SURVEILLANCE.AGE.MAPPING$`All ages`,
        data.types='prevalence')

simplot(simset.new, 
        years=1980:2030, 
        facet.by=c('age',"sex"), 
        ages = "15+", 
        data.types='incidence')

simplot(simset.new, 
        years=1980:2030, 
        facet.by=c('age',"sex"), 
        ages = "15+", 
        data.types='prevalence')

simplot(simset.new, 
        years=1980:2030, 
        facet.by=c('age',"sex"), 
        ages = "15+", 
        data.types='hiv.mortality')


simplot(simset.new, #@simulations[[simset.new@n.sim]],  
        years=1980:2030, 
        data.types='total.mortality')

simplot(simset.new, 
        years=1980:2020, 
        data.types='engagement', 
        proportion=T)

simplot(simset.new, 
        years=1980:2020, 
        facet.by=c('age','sex'), 
        data.types='engagement', 
        proportion=T)

simplot(simset.new,
        sim.new.last,
        years=1980:2020, 
        data.types='suppression', 
        proportion=T)

simplot(simset.new, 
        years=1980:2020, 
        facet.by=c('age','sex'),
        data.types='suppression', 
        proportion=T)

simplot(simset.new, 
        years=1980:2020, 
        facet.by=c('age'),
        ages = MODEL.TO.SURVEILLANCE.AGE.MAPPING$`All ages`,
        data.types='suppression', 
        proportion=T)


# THESE PLOTS DON'T WORK
simplot(simset.new.test, 
        years=1980:2030, 
        facet.by=c('age'), 
        data.types='hiv.mortality', 
        proportion = T,
        show.individual.sims = F)

simplot(simset.new.test,
        years=2010:2040, 
        data.types=c('awareness',"engagement","suppression"), 
        proportion=T, 
        show.individual.sims = F)

simplot(simset.new.test,
        years=2010:2040, 
        data.types=c('awareness',"engagement","suppression"), 
        facet.by=c('age','sex'), 
        proportion=T, 
        show.individual.sims = F)

simplot(simset.new.test, 
        years=1980:2040, 
        data.types='awareness', 
        proportion=T)
simplot(simset.new.test, 
        years=1980:2040, 
        facet.by=c('age','sex'), 
        data.types='awareness', 
        proportion=T, 
        show.individual.sims = F)


# 
# load("mcmc_runs/simset_france_2025-02-06.Rdata")
# simset.old = simset
# sim.old.last = simset.old@simulations[[simset.old@n.sim]]