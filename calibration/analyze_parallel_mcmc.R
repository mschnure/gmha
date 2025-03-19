source("model/run_systematic.R")

#load("mcmc_runs/simset_kenya_2025-02-27.Rdata")
#load("mcmc_runs/simset_south_africa_2025-02-27.Rdata")
#load("mcmc_runs/simset_france_2025-02-27.Rdata")

simplot(simset,
        years = 1980:2030)

simplot(simset, #@simulations[[simset@n.sim]],
        years = 1980:2030, 
        data.types = "population")

simplot(simset, #@simulations[[simset@n.sim]],
        years = 1980:2030, 
        data.types = "population",
        facet.by='age')

simplot(simset, 
        years=1980:2030, 
        facet.by='age', 
        #ages = MODEL.TO.SURVEILLANCE.AGE.MAPPING$`All ages`,
        data.types='incidence')

simplot(simset, 
        years=1980:2030, 
        facet.by='age', 
        #ages = MODEL.TO.SURVEILLANCE.AGE.MAPPING$`All ages`,
        data.types='prevalence')

simplot(simset, 
        years=1980:2030, 
        facet.by=c('age'), 
        #ages = MODEL.TO.SURVEILLANCE.AGE.MAPPING$`All ages`,
        data.types='hiv.mortality')

simplot(simset, 
        years=1980:2030, 
        facet.by=c('age',"sex"), 
        ages = "15+", 
        data.types='incidence')

simplot(simset, 
        years=1980:2030, 
        facet.by=c('age',"sex"), 
        ages = "15+", 
        data.types='prevalence')

simplot(simset, 
        years=1980:2030, 
        facet.by=c('age',"sex"), 
        ages = "15+", 
        data.types='hiv.mortality')


simplot(simset, #@simulations[[simset@n.sim]],  
        years=1980:2030, 
        data.types='total.mortality')

simplot(simset, #@simulations[[simset@n.sim]],
        years=1980:2030, 
        facet.by=c('age'), 
        #sexes = "male",
        data.types='total.mortality')

simplot(simset,
        years=2010:2030, 
        data.types=c('awareness',"engagement","suppression"), 
        proportion=T)

simplot(simset,
        years=2010:2030, 
        data.types=c('awareness',"engagement","suppression"), 
        facet.by=c('age','sex'), 
        proportion=T)

simplot(simset, 
        years=1980:2020, 
        data.types='engagement', 
        proportion=T)

simplot(simset, 
        years=1980:2020, 
        facet.by=c('age','sex'), 
        data.types='engagement', 
        proportion=T)

simplot(simset, 
        years=1980:2020, 
        data.types='suppression', 
        proportion=T)

simplot(simset, 
        years=1980:2020, 
        facet.by=c('age','sex'),
        data.types='suppression', 
        proportion=T)

simplot(simset, 
        years=1980:2030, 
        data.types='awareness', 
        proportion=T)

simplot(simset, 
        years=1980:2030, 
        facet.by=c('age','sex'), 
        data.types='awareness', 
        proportion=T)




