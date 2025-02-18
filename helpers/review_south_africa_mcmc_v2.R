source("model/run_systematic.R")

#source("calibration/likelihood/individual_likelihoods.R") # make sure it's set to South Africa

load("mcmc_runs/simset_south_africa_2025-02-14.Rdata")
sim.mcmc = simset@simulations[[simset@n.sim]]
sim.first = simset@simulations[[1]]

params.mcmc = simset@parameters[simset@n.sim,]
params.first = simset@parameters[1,]
params.manual = params.mcmc

#params.manual["birth.transmission.risk.0"] 


sim.manual = run.model.for.parameters(variable.parameters = params.manual,
                                      end.year = 2040,
                                      location = sim.mcmc$location)
simplot(sim.first,
        sim.mcmc, 
    #sim.manual, 
    #simset,
    years=1980:2030, 
    facet.by='age', 
    ages = MODEL.TO.SURVEILLANCE.AGE.MAPPING$`All ages`,
    data.types='prevalence')

simplot(sim.mcmc, 
        #sim.manual,
        years=1980:2030, 
        facet.by='age', 
        #ages = MODEL.TO.SURVEILLANCE.AGE.MAPPING$`All ages`,
        data.types='incidence')



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

likelihood.to.run = create.likelihood(parameters = create.model.parameters(location = sim.manual$location),
                                      location=sim.manual$location)

exp(likelihood.to.run(sim.manual) - likelihood.to.run(sim.mcmc))
exp(full.lik(sim.manual) - full.lik(sim.mcmc))
exp(pop.lik(sim.manual) - pop.lik(sim.mcmc))
exp(incidence.lik(sim.manual) - incidence.lik(sim.mcmc)) # this is worse? 
exp(prev.lik(sim.manual) - prev.lik(sim.mcmc))
exp(aware.lik(sim.manual) - aware.lik(sim.mcmc))
exp(eng.lik(sim.manual) - eng.lik(sim.mcmc))
exp(supp.lik(sim.manual) - supp.lik(sim.mcmc)) 
exp(hiv.mortality.lik(sim.manual) - hiv.mortality.lik(sim.mcmc))
exp(aware.trend.lik(sim.manual) - aware.trend.lik(sim.mcmc))
exp(total.mortality.lik(sim.manual) - total.mortality.lik(sim.mcmc)) 
