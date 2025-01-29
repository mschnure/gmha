source("model/run_systematic.R")

#load("~/Library/CloudStorage/Dropbox/Documents_local/Hopkins/SOM_Job/3_Aging_multimorbidity/gmha/mcmc_runs/mcmc_test_2024-12-13.Rdata")
#load("~/Library/CloudStorage/Dropbox/Documents_local/Hopkins/SOM_Job/3_Aging_multimorbidity/gmha/mcmc_runs/mcmc_test_2024-12-17.Rdata")

# simset.test = extract.simset(mcmc.test,
#                              additional.burn=500,
#                              additional.thin=20)
# 
# save(simset.test,file=paste0("mcmc_runs/simset.test_",LOCATION.FOR.SAVING,"_",Sys.Date(),".Rdata"))

load("mcmc_runs/simset_kenya_2025-01-28.Rdata")
#load("mcmc_runs/simset_south_africa_2025-01-28.Rdata")
#load("mcmc_runs/simset_france_2025-01-19.Rdata")


simplot(simset,
        years = 1980:2030, 
        show.individual.sims = F)

simplot(simset, #@simulations[[simset@n.sim]],
        years = 1980:2030, 
        data.types = "population",
        show.individual.sims = F)

simplot(simset, #@simulations[[simset@n.sim]],
        years = 1980:2030, 
        data.types = "population",
        facet.by='age', 
        show.individual.sims = F)

simplot(simset, 
        years=1980:2030, 
        facet.by='age', 
        #ages = MODEL.TO.SURVEILLANCE.AGE.MAPPING$`All ages`,
        data.types='incidence', 
        show.individual.sims = F)

simplot(simset, 
        years=1980:2030, 
        facet.by='age', 
        #ages = MODEL.TO.SURVEILLANCE.AGE.MAPPING$`All ages`,
        data.types='prevalence', 
        show.individual.sims = F)

simplot(simset, 
        years=1980:2030, 
        facet.by=c('age'), 
        #ages = MODEL.TO.SURVEILLANCE.AGE.MAPPING$`All ages`,
        data.types='hiv.mortality', 
        show.individual.sims = F)

simplot(simset, 
        years=1980:2030, 
        facet.by=c('age',"sex"), 
        ages = "15+", 
        data.types='incidence', 
        show.individual.sims = F)

simplot(simset, 
        years=1980:2030, 
        facet.by=c('age',"sex"), 
        ages = "15+", 
        data.types='prevalence', 
        show.individual.sims = F)

simplot(simset, #@simulations[[simset@n.sim]],  
        years=1980:2030, 
        data.types='total.mortality', 
        show.individual.sims = F)

simplot(simset, #@simulations[[simset@n.sim]],
        years=1980:2030, 
        facet.by=c('age'), 
        #sexes = "male",
        data.types='total.mortality', 
        show.individual.sims = F)

simplot(simset, 
        years=1980:2020, 
        data.types='engagement', 
        proportion=T,
        show.individual.sims = F)

simplot(simset, 
        years=1980:2020, 
        facet.by=c('age','sex'), 
        data.types='engagement', 
        proportion=T,
        show.individual.sims = F)

simplot(simset, 
        years=1980:2020, 
        data.types='suppression', 
        proportion=T,
        show.individual.sims = F)

simplot(simset, 
        years=1980:2020, 
        facet.by=c('age','sex'),
        data.types='suppression', 
        proportion=T,
        show.individual.sims = F)


# THESE PLOTS DON'T WORK
simplot(simset.test, 
        years=1980:2030, 
        facet.by=c('age'), 
        data.types='hiv.mortality', 
        proportion = T,
        show.individual.sims = F)

simplot(simset.test,
        years=2010:2040, 
        data.types=c('awareness',"engagement","suppression"), 
        proportion=T, 
        show.individual.sims = F)

simplot(simset.test,
        years=2010:2040, 
        data.types=c('awareness',"engagement","suppression"), 
        facet.by=c('age','sex'), 
        proportion=T, 
        show.individual.sims = F)

simplot(simset.test, 
        years=1980:2040, 
        data.types='awareness', 
        proportion=T)
simplot(simset.test, 
        years=1980:2040, 
        facet.by=c('age','sex'), 
        data.types='awareness', 
        proportion=T, 
        show.individual.sims = F)


