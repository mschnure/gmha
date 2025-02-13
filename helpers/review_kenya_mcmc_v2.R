source("model/run_systematic.R")

source("calibration/likelihood/individual_likelihoods.R") # make sure it's set to Kenya

load("mcmc_runs/simset_kenya_2025-02-13.Rdata")
sim.mcmc = simset@simulations[[simset@n.sim]]

params.mcmc = simset@parameters[simset@n.sim,]
params.manual = params.mcmc

params.manual["birth.transmission.risk.0"] = 0.6 # arrived at 0.3165901, started at 0.42 - WAS GOOD AT 0.5 AND NO TRANSMISSION CHANGES 
params.manual["birth.transmission.risk.1"] # arrived at 0.4019852, started at 0.3

params.manual["age.0.to.4.hiv.mortality.multiplier.0"] = 3 # 1.107794
params.manual["age.5.to.14.hiv.mortality.multiplier.0"] = 3 # 1.553792
params.manual["age.0.to.14.hiv.mortality.multiplier.1"] = 3 # 1.227954
params.manual["age.0.to.14.hiv.mortality.multiplier.2"] # 5.485132

params.manual["age.15.to.19.transmission.multiplier.0"] = 0.9 # 1.029681
params.manual["age.15.to.19.transmission.multiplier.1"] = 0.4 # 0.352174
params.manual["age.15.to.19.transmission.multiplier.2"] = 0.3 # 0.2542788
params.manual["age.15.to.19.transmission.multiplier.3"] # 1.756702

params.manual["age.40.to.49.transmission.multiplier.0"] = 0.95 # 0.8843208
params.manual["age.40.to.49.transmission.multiplier.1"] = 4.5 # 0.3738723
params.manual["age.40.to.49.transmission.multiplier.2"] = 1 # 0.9198105
params.manual["age.40.to.49.transmission.multiplier.3"] # 2.564686

sim.manual = run.model.for.parameters(variable.parameters = params.manual,
                                      end.year = 2040,
                                      location = sim.mcmc$location)
simplot(sim.mcmc, 
        sim.manual,
        years=1980:2030, 
        facet.by='age', 
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
