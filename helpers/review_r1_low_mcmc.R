source("model/run_systematic.R")

load("mcmc_runs/simset_r1.low_2025-08-09.Rdata")
sim.mcmc = simset@simulations[[simset@n.sim]]
params.mcmc = simset@parameters[simset@n.sim,]

sim.mcmc = run.model.for.parameters(variable.parameters = params.mcmc,
                                    end.year = 2040,
                                    location = sim.mcmc$location)

params.manual = params.mcmc
cbind(params.manual)

params.manual["age.50.and.over.transmission.multiplier.0"] = 0.3 # 0.2059075 
params.manual["age.50.and.over.transmission.multiplier.1"] = 0.3 # 0.2109254 
params.manual["age.50.and.over.transmission.multiplier.2"] = 0.2 # 0.4094585 
params.manual["age.50.and.over.transmission.multiplier.3"] = 0.1 # 0.4155778 

sim.manual = run.model.for.parameters(variable.parameters = params.manual,
                                      end.year = 2040,
                                      location = sim.mcmc$location)
simplot(sim.mcmc, 
        sim.manual,
        years=1980:2040)

simplot(sim.mcmc, 
        sim.manual,
        years=1980:2040, 
        facet.by='age', 
        #ages = MODEL.TO.SURVEILLANCE.AGE.MAPPING$`All ages`,
        data.types='incidence')

simplot(sim.mcmc, 
        sim.manual, 
        #simset,
        years=1980:2040, 
        facet.by='age', 
        #ages = MODEL.TO.SURVEILLANCE.AGE.MAPPING$`All ages`,
        data.types='prevalence')

simplot(sim.mcmc, 
        sim.manual, 
        years=1980:2040, 
        facet.by='age', 
        # ages = MODEL.TO.SURVEILLANCE.AGE.MAPPING$`All ages`,
        data.types='hiv.mortality')

simplot(sim.mcmc, 
        sim.manual, 
        years = 1980:2030, 
        data.types = "population",
        #sexes = "female",
        facet.by='age')

simplot(sim.mcmc, 
        sim.manual, 
        years = 1980:2030, 
        data.types = "total.mortality",
        facet.by='age')

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
