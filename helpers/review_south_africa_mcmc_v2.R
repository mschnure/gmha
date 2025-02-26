source("model/run_systematic.R")

source("calibration/likelihood/individual_likelihoods.R") # make sure it's set to South Africa

load("mcmc_runs/simset_south_africa_2025-02-26.Rdata")
sim.mcmc = simset@simulations[[simset@n.sim]]
params.mcmc = simset@parameters[simset@n.sim,]
params.mcmc["age.25.to.49.hiv.mortality.multiplier.0"] = 1
params.mcmc["age.25.to.49.hiv.mortality.multiplier.1"] = 1
params.mcmc["age.25.to.49.hiv.mortality.multiplier.2"] = 1

sim.mcmc = run.model.for.parameters(variable.parameters = params.mcmc,
                                      end.year = 2040,
                                      location = sim.mcmc$location)

params.manual = params.mcmc
cbind(params.manual)

params.manual["over.80.mortality.intercept.multiplier.male"] = 1.5 #1.004108
params.manual["over.80.mortality.intercept.multiplier.female"] = 1.4 #0.7921224 (or 1.1) - if I go with 1.4, downweight 80+ mortality even more

params.manual["age.15.to.24.hiv.mortality.multiplier.1"] = 0.5 # 0.246964410
params.manual["age.15.to.24.hiv.mortality.multiplier.2"] = 1 # 0.736734

params.manual["age.25.to.49.hiv.mortality.multiplier.1"] = 0.6
params.manual["age.25.to.49.hiv.mortality.multiplier.2"] = 0.01

params.manual["over.50.hiv.mortality.multiplier.0"] = 25 # 0.555852490
params.manual["over.50.hiv.mortality.multiplier.1"] = 25 # 4.28453
params.manual["over.50.hiv.mortality.multiplier.2"] = 25 # 1.139815427

params.manual["age.25.to.50.aging.factor"] = 1 # 0.793711697

# params.manual["age.50.and.over.transmission.multiplier.1"] = 0.75 # 0.477120959, 1997
# params.manual["age.50.and.over.transmission.multiplier.2"] = 0.55 # 0.479667919, 2008
# params.manual["age.50.and.over.transmission.multiplier.3"] = 0.65 # 0.835682624, 2018 

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
