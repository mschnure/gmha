source("model/run_systematic.R")

source("calibration/likelihood/individual_likelihoods.R") # make sure it's set to France

load("mcmc_runs/simset_france_2025-02-19.Rdata")
sim.mcmc = simset@simulations[[simset@n.sim]]
sim.first = simset@simulations[[1]]

params.mcmc = simset@parameters[simset@n.sim,]
params.manual = params.mcmc

#params.manual[""] 


sim.manual = run.model.for.parameters(variable.parameters = params.manual,
                                      end.year = 2040,
                                      location = sim.mcmc$location)
simplot(sim.first,
        sim.mcmc,
        years = 1980:2030)

simplot(sim.first, 
        sim.mcmc,
        years=1980:2030, 
        #facet.by='age', 
        #ages = MODEL.TO.SURVEILLANCE.AGE.MAPPING$`All ages`,
        data.types='incidence')

simplot(sim.mcmc, 
        sim.manual, 
        years=1980:2030, 
        facet.by='age', 
        # ages = MODEL.TO.SURVEILLANCE.AGE.MAPPING$`All ages`,
        data.types='prevalence')

simplot(sim.mcmc, 
        sim.manual, 
        years=1980:2030, 
        facet.by='age', 
        # ages = MODEL.TO.SURVEILLANCE.AGE.MAPPING$`All ages`,
        data.types='hiv.mortality')

simplot(sim.mcmc, 
        sim.manual, 
        years = 1980:2030, 
        data.types = "population",
        facet.by='age')

simplot(sim.mcmc, 
        sim.manual, 
        years = 1980:2030, 
        data.types = "total.mortality",
        facet.by='age')

likelihood.to.run = create.likelihood(parameters = create.model.parameters(location = sim.mcmc$location),
                                      location=sim.mcmc$location)

# changed weight in individual likelihoods code to 0.1 - need to change if I want to test these out again 
exp(likelihood.to.run(sim.mcmc) - likelihood.to.run(sim.first))
exp(full.lik(sim.mcmc) - full.lik(sim.first))
exp(pop.lik(sim.mcmc) - pop.lik(sim.first))
exp(incidence.lik(sim.mcmc) - incidence.lik(sim.first)) 
exp(prev.lik(sim.mcmc) - prev.lik(sim.first))
exp(aware.lik(sim.mcmc) - aware.lik(sim.first))
exp(eng.lik(sim.mcmc) - eng.lik(sim.first))
exp(supp.lik(sim.mcmc) - supp.lik(sim.first)) 
exp(hiv.mortality.lik(sim.mcmc) - hiv.mortality.lik(sim.first))
exp(aware.trend.lik(sim.mcmc) - aware.trend.lik(sim.first))
exp(total.mortality.lik(sim.mcmc) - total.mortality.lik(sim.first)) 
