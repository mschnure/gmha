
load("mcmc_runs/simset_kenya_2025-02-09.Rdata")
sim.mcmc = simset@simulations[[simset@n.sim]]

simplot(sim.mcmc, 
        years=1980:2030, 
        facet.by='age', 
        #ages = MODEL.TO.SURVEILLANCE.AGE.MAPPING$`All ages`,
        data.types='incidence')

simplot(sim.mcmc, 
        years=1980:2030, 
        facet.by='age', 
       # ages = MODEL.TO.SURVEILLANCE.AGE.MAPPING$`All ages`,
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
        # ages = MODEL.TO.SURVEILLANCE.AGE.MAPPING$`All ages`,
        data.types='prevalence')

plot.age.distribution(sim.manual,
                      plot.limits=c(0,35000))

simplot(sim.mcmc, 
        sim.manual,
        years = 1980:2030, 
        data.types = "population",
        facet.by='age')
