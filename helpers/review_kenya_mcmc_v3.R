source("model/run_systematic.R")

source("calibration/likelihood/individual_likelihoods.R") # make sure it's set to Kenya

#load("mcmc_runs/simset_kenya_2025-02-19.Rdata")
load("cached/all.results_kenya_2025-02-19.Rdata")
load("calibration/starting_values/2025_02_13_kenya_start_values.Rdata")

simset = simset.no.int
sim.mcmc = simset@simulations[[simset@n.sim]]

params.mcmc = simset@parameters[simset@n.sim,]
params.manual = params.mcmc
params.default = get.default.parameters("Kenya")
#params.manual["log.OR.suppression.slope"] = -0.01 #0.3081713

params.start.values["birth.transmission.risk.0"] # 0.6
params.start.values["birth.transmission.risk.1"] # 0.4019852

params.default["birth.transmission.risk.0"] # 0.42
params.default["birth.transmission.risk.1"] # 0.3

params.manual["birth.transmission.risk.0"] = 0.5 # 0.4041279
params.manual["birth.transmission.risk.1"] # 0.5026461

sim.manual = run.model.for.parameters(variable.parameters = params.manual,
                                      end.year = 2040,
                                      location = sim.mcmc$location)

simplot(simset.no.int,
        sim.mcmc,
        sim.manual,
        years=1980:2040, 
        facet.by='age', 
        #ages = MODEL.TO.SURVEILLANCE.AGE.MAPPING$`All ages`,
        data.types='incidence')

simplot(simset.no.int,
        sim.mcmc,
        sim.manual,
        years=1980:2040, 
        facet.by='age', 
        #ages = MODEL.TO.SURVEILLANCE.AGE.MAPPING$`All ages`,
        data.types='prevalence')

simplot(sim.mcmc, 
        sim.manual,
        years=1980:2040, 
        facet.by=c('age','sex'),
        data.types='suppression', 
        proportion=T)

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
        sexes = "female",
        data.types = "total.mortality",
        facet.by='age')

likelihood.to.run = create.likelihood(parameters = create.model.parameters(location = sim.manual$location),
                                      location=sim.manual$location)

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
