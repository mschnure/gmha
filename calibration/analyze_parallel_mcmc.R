source("model/run_systematic.R")

#load("~/Library/CloudStorage/Dropbox/Documents_local/Hopkins/SOM_Job/3_Aging_multimorbidity/gmha/mcmc_runs/mcmc_test_2024-12-13.Rdata")
#load("~/Library/CloudStorage/Dropbox/Documents_local/Hopkins/SOM_Job/3_Aging_multimorbidity/gmha/mcmc_runs/mcmc_test_2024-12-17.Rdata")

# simset.test = extract.simset(mcmc.test,
#                              additional.burn=500,
#                              additional.thin=20) 
# 
# save(simset.test,file=paste0("mcmc_runs/simset.test_",LOCATION.FOR.SAVING,"_",Sys.Date(),".Rdata"))

load("~/Library/CloudStorage/Dropbox/Documents_local/Hopkins/SOM_Job/3_Aging_multimorbidity/gmha/mcmc_runs/simset.test_south_africa_2024-12-18.Rdata")

simplot(simset.test,  
        years = 1980:2030, 
        show.individual.sims = F)

simplot(simset.test,  
        years = 1980:2030, 
        data.types = "population",
        show.individual.sims = F)

simplot(simset.test,  
        years = 1980:2030, 
        data.types = "population",
        facet.by='age', 
        show.individual.sims = F)

simplot(simset.test, 
        years=1980:2030, 
        facet.by='age', 
        data.types='incidence', 
        show.individual.sims = F)

simplot(simset.test, 
        years=1980:2030, 
        facet.by='age', 
        data.types='prevalence', 
        show.individual.sims = F)

simplot(simset.test, 
        years=1980:2030, 
        facet.by=c('age',"sex"), 
        ages = "15+", 
        data.types='prevalence', 
        show.individual.sims = F)

simplot(simset.test, 
        years=1980:2030, 
        facet.by=c('age',"sex"), 
        ages = "15+", 
        data.types='incidence', 
        show.individual.sims = F)

simplot(simset.kenya, 
        years=1980:2030, 
        facet.by=c('age'), 
        data.types='hiv.mortality', 
        show.individual.sims = F)

simplot(simset.no.int.28.sampled, sub.simset, years=1980:2020, facet.by='age', data.types='hiv.mortality',proportion = T, show.individual.sims = F)
simplot(simset.29.sampled, years=2010:2040, data.types=c('awareness',"engagement","suppression"), proportion=T, show.individual.sims = F)
simplot(simset.no.int.28.sampled, sub.simset, years=2010:2040, data.types=c('awareness',"engagement","suppression"), facet.by=c('age','sex'), proportion=T, show.individual.sims = F)
simplot(simset.30.sampled, years=1980:2040, data.types='awareness', proportion=T)
simplot(simset.29.sampled, years=1980:2040, facet.by=c('age','sex'), data.types='awareness', proportion=T)
simplot(simset.29.sampled, years=1980:2040, facet.by=c('age','sex'), data.types='awareness', proportion=T, show.individual.sims = F)

simplot(simset, years=1980:2020, facet.by=c('age','sex'), data.types='engagement', proportion=T)
simplot(simset, years=1980:2020, facet.by=c('age','sex'), data.types='suppression', proportion=T)

simplot(simset.no.int.28.sampled, sub.simset, years=1980:2040, facet.by='age', data.types='population', show.individual.sims = F)
simplot(simset.no.int.23, years=1980:2030, facet.by='age', data.types='population', 
        ages = c("60-64","65-69","70-74","75-79","80 and over"), show.individual.sims = F)

