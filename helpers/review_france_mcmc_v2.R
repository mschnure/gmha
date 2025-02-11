
load("mcmc_runs/simset_france_2025-02-11.Rdata")
sim.mcmc = simset@simulations[[simset@n.sim]]

simplot(sim.mcmc, 
        years=1980:2030, 
        facet.by='age', 
        ages = MODEL.TO.SURVEILLANCE.AGE.MAPPING$`All ages`,
        data.types='prevalence')

params.mcmc = simset@parameters[simset@n.sim,]
params.manual = params.mcmc

cbind(params.manual)
# params.manual["age.15.to.19.aging.factor"] = 1 #0.024593083
# params.manual["age.20.to.24.aging.factor"] = 1 #0.233335955
params.manual["age.25.to.50.aging.factor"] = 1 #0.798845362

## MIGHT NEED TO FURTHER RESTRICT OR REMOVE THESE AGING FACTORS 

sim.manual = run.model.for.parameters(variable.parameters = params.manual,
                                      end.year = 2040,
                                      location = sim.mcmc$location)

simplot(sim.mcmc, 
        sim.manual,
        years=1980:2030, 
        facet.by='age', 
        #ages = MODEL.TO.SURVEILLANCE.AGE.MAPPING$`All ages`,
        data.types='prevalence')

simplot(sim.mcmc, 
        sim.manual,
        years=1980:2030, 
        facet.by='age', 
        # ages = MODEL.TO.SURVEILLANCE.AGE.MAPPING$`All ages`,
        data.types='incidence')

plot.age.distribution(sim.manual,
                      plot.limits=c(0,35000))

simplot(sim.mcmc, 
        sim.manual,
        years = 1980:2030, 
        data.types = "population",
        facet.by='age')

source("calibration/likelihood/individual_likelihoods.R") # make sure to change country to France before sourcing 
likelihood.to.run = create.likelihood(parameters = create.model.parameters(location = "France"),
                                      location="France")

exp(likelihood.to.run(sim.manual) - likelihood.to.run(sim.mcmc))
exp(full.lik(sim.manual) - full.lik(sim.mcmc))
exp(pop.lik(sim.manual) - pop.lik(sim.mcmc))
exp(incidence.lik(sim.manual) - incidence.lik(sim.mcmc)) # this is worse; what if I removed 2x weight
exp(prev.lik(sim.manual) - prev.lik(sim.mcmc))
exp(aware.lik(sim.manual) - aware.lik(sim.mcmc)) # also worse 
exp(eng.lik(sim.manual) - eng.lik(sim.mcmc))
exp(supp.lik(sim.manual) - supp.lik(sim.mcmc)) 
exp(hiv.mortality.lik(sim.manual) - hiv.mortality.lik(sim.mcmc))
exp(aware.trend.lik(sim.manual) - aware.trend.lik(sim.mcmc))
exp(total.mortality.lik(sim.manual) - total.mortality.lik(sim.mcmc)) 
