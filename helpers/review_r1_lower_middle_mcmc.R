source("model/run_systematic.R")

source("calibration/likelihood/individual_likelihoods.R") # make sure it's set to r1.lower.middle

load("mcmc_runs/simset_r1.lower.middle_2025-08-09.Rdata")
sim.mcmc = simset@simulations[[simset@n.sim]]
params.mcmc = simset@parameters[simset@n.sim,]

sim.mcmc = run.model.for.parameters(variable.parameters = params.mcmc,
                                    end.year = 2040,
                                    location = sim.mcmc$location)

params.manual = params.mcmc
cbind(params.manual)

# params.manual["age.15.to.19.transmission.multiplier.1"] = 0.6 # 0.5186193
# params.manual["age.15.to.19.transmission.multiplier.2"] # 1.136691
# params.manual["age.15.to.19.transmission.multiplier.3"] = 1 # 0.730659866

params.manual["age.50.and.over.transmission.multiplier.0"] = 0.35 # 0.1130542
# params.manual["age.50.and.over.transmission.multiplier.1"] # 0.3526805
# params.manual["age.50.and.over.transmission.multiplier.2"] # 0.216003
params.manual["age.50.and.over.transmission.multiplier.3"] = 0.1 # 0.847161642

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
