source("model/run_systematic.R")

source("calibration/likelihood/individual_likelihoods.R") # make sure it's set to correct country

load("cached/all.results_non.unaids.remainder_2025-04-23.Rdata")
simset = simset.list.full$no.int

sim.mcmc = simset@simulations[[simset@n.sim]]
params.mcmc = simset@parameters[simset@n.sim,]
params.first = simset@parameters[1,]

sim.mcmc = run.model.for.parameters(variable.parameters = params.mcmc,
                                    end.year = 2040,
                                    location = sim.mcmc$location)

params.manual = params.mcmc
cbind(params.first,params.mcmc)

params.start.values = get.default.parameters(location = sim.mcmc$location)

params.start.values["trate.0"] = .41 # 0.8
params.start.values["trate.1"] = .45 # 0.1
params.start.values["trate.2"] = .15 # 0.1
params.start.values["trate.3"] = .12 # 0.1
params.start.values["trate.4"] = .12 # 0.1

sim.start.values = run.model.for.parameters(variable.parameters = params.start.values,
                                            end.year = 2040,
                                            location = sim.mcmc$location)

params.manual["trate.0"] = .41 # 0.67626194
params.manual["trate.1"] = .45 # 0.07178398
params.manual["trate.2"] = .15 # 0.05468648
params.manual["trate.3"] = .12 # 0.1313023
params.manual["trate.4"] = .12 # 0.4058995

sim.manual = run.model.for.parameters(variable.parameters = params.manual,
                                      end.year = 2040,
                                      location = sim.mcmc$location)

simplot(sim.start.values,
        sim.mcmc, 
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
