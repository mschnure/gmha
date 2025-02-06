### 2/5
load("mcmc_runs/simset_kenya_2025-02-02.Rdata")
#load("mcmc_runs/simset_south_africa_2025-02-03.Rdata")
#load("mcmc_runs/simset_france_2025-01-19.Rdata")
sim.last = simset@simulations[[simset@n.sim]]
sim.first = simset@simulations[[1]]

params.mcmc = simset@parameters[simset@n.sim,]
params.manual = params.mcmc

sim.manual = sim.last
cbind(params.manual)
params.manual["age.50.and.over.transmission.multiplier.0"] = 1 # 1.213366250
params.manual["age.50.and.over.transmission.multiplier.1"] = 1 # 0.890353383
params.manual["age.50.and.over.transmission.multiplier.3"] = 1 # 0.527950781

params.manual["age.20.to.29.transmission.multiplier.0"] = 1.3 # 1.211353825
params.manual["age.20.to.29.transmission.multiplier.1"] = 1 # 0.858479764
#params.manual["age.20.to.29.transmission.multiplier.2"] = 1 # 1.809024437
params.manual["age.20.to.29.transmission.multiplier.3"] = 1 # 0.583984377

params.manual["age.40.to.49.transmission.multiplier.0"] = 1 # 0.875790144
params.manual["age.40.to.49.transmission.multiplier.1"] = 1 # 0.337859867
params.manual["age.40.to.49.transmission.multiplier.2"] = 1 # 0.314069025
params.manual["age.40.to.49.transmission.multiplier.3"] = 1 # 1.846459710

# params.manual["suppression.rate.0"] = 1.118678 # 2.401848844
# params.manual["suppression.rate.1"] = 1.118678 # 5.109340399

sim.manual = run.model.for.parameters(variable.parameters = params.manual,
                                      end.year = 2030,
                                      location = sim.last$location)
simplot(sim.last,
        sim.manual,
        years=1980:2030, 
        facet.by='age', 
        data.types='incidence')

simplot(sim.last,
        sim.manual,
        years=1980:2030, 
        facet.by='age', 
        data.types='prevalence')

simplot(sim.last,
        sim.manual,
        years = 1980:2030)

simplot(sim.last,
        sim.manual,
        years=1980:2030, 
        facet.by='age', 
        data.types='hiv.mortality')

simplot(sim.last,
        sim.manual, 
        years=1980:2020, 
        data.types='suppression', 
        proportion=T)

simplot(sim.last,
        sim.manual, 
        years=1980:2020, 
        data.types='engagement', 
        proportion=T)

exp(likelihood.to.run(sim.last) - likelihood.to.run(sim.manual))
exp(full.lik(sim.last) - full.lik(sim.manual))
exp(pop.lik(sim.last) - pop.lik(sim.manual))
exp(incidence.lik(sim.last) - incidence.lik(sim.manual)) 
exp(prev.lik(sim.last) - prev.lik(sim.manual))
exp(aware.lik(sim.last) - aware.lik(sim.manual))
exp(eng.lik(sim.last) - eng.lik(sim.manual))
exp(supp.lik(sim.last) - supp.lik(sim.manual)) 
exp(hiv.mortality.lik(sim.last) - hiv.mortality.lik(sim.manual))
exp(aware.trend.lik(sim.last) - aware.trend.lik(sim.manual))
exp(total.mortality.lik(sim.last) - total.mortality.lik(sim.manual)) 

simplot(simset,
        sim.first,
        sim.last,
        years = 1980:2030)

simplot(simset,
        sim.first,
        sim.last,
        ages = c("50 and over"),
        facet.by = "age",
        data.types = c("incidence","prevalence","hiv.mortality"),
        years = 1980:2030)

simplot(simset, 
        sim.first,
        sim.last,
        years = 1980:2030, 
        data.types = "population")

simplot(simset, 
        sim.first,
        sim.last,
        years = 1980:2030, 
        data.types = "population",
        #ages = c("60-64","65-69","70-74","75-79","80 and over"),
        facet.by='age')

simplot(simset, 
        sim.first,
        sim.last,
        years=1980:2030, 
        facet.by=c('age'), 
        #sexes = "male",
        data.types='total.mortality')



simplot(simset, 
        sim.first,
        sim.last,
        years=1980:2030, 
        facet.by='age', 
        #ages = MODEL.TO.SURVEILLANCE.AGE.MAPPING$`All ages`,
        data.types='prevalence')

simplot(simset, 
        sim.first,
        sim.last,
        years=1980:2030, 
        facet.by=c('age'), 
        #ages = MODEL.TO.SURVEILLANCE.AGE.MAPPING$`All ages`,
        data.types='hiv.mortality')

simplot(simset, 
        sim.first,
        sim.last,
        years=1980:2030, 
        facet.by=c('age',"sex"), 
        ages = "15+", 
        data.types='hiv.mortality')


likelihood.to.run = create.likelihood(parameters = create.model.parameters(location = "Kenya"),
                                      location="Kenya")

exp(likelihood.to.run(sim.last) - likelihood.to.run(sim.first))
exp(full.lik(sim.last) - full.lik(sim.first))
exp(pop.lik(sim.last) - pop.lik(sim.first))
exp(incidence.lik(sim.last) - incidence.lik(sim.first)) 
exp(prev.lik(sim.last) - prev.lik(sim.first))
exp(aware.lik(sim.last) - aware.lik(sim.first))
exp(eng.lik(sim.last) - eng.lik(sim.first))
exp(supp.lik(sim.last) - supp.lik(sim.first)) 
exp(hiv.mortality.lik(sim.last) - hiv.mortality.lik(sim.first))
exp(aware.trend.lik(sim.last) - aware.trend.lik(sim.first))
exp(total.mortality.lik(sim.last) - total.mortality.lik(sim.first)) 



### 1/30 

#load("mcmc_runs/simset_kenya_2025-01-28.Rdata")
# sim.kenya = simset@simulations[[simset@n.sim]]
# params.kenya = simset@parameters[[simset@n.sim]]
# sim.kenya.projected = run.model.for.parameters(variable.parameters = params.kenya,
#                                                end.year = 2040,
#                                                location = sim.kenya$location)

load("mcmc_runs/simset_south_africa_2025-01-28.Rdata")
sim.mcmc = simset@simulations[[simset@n.sim]]
params.mcmc = simset@parameters[simset@n.sim,]
params.manual = params.mcmc

sim.mcmc.projected = run.model.for.parameters(variable.parameters = params.mcmc,
                                              end.year = 2040,
                                              location = sim.mcmc$location)

# apply(sim.mcmc.projected$population, "year", sum)
# apply(sim.kenya.projected$population, "year", sum)

sim.manual = sim.mcmc.projected

#cbind(params.manual)

if(1==2){ # Kenya changes 
    params.manual["over.80.mortality.intercept.multiplier.male"] = 1 # 0.84194077
    params.manual["over.80.mortality.intercept.multiplier.female"] = 1 # 1.00302635
    params.manual["over.80.mortality.slope.multiplier.male"] = 1 # 0.98877095
    params.manual["over.80.mortality.slope.multiplier.female"] = 1 # 0.98227020    
}
if(1==2){ # South Africa changes
    params.manual["over.80.mortality.intercept.multiplier.male"] = 3 # 2.158523
    params.manual["over.80.mortality.intercept.multiplier.female"] = 1 # 0.5530541
    params.manual["over.80.mortality.slope.multiplier.male"] = 1 # 0.9682139
    params.manual["over.80.mortality.slope.multiplier.female"] = 1 # 1.006965        
}

sim.manual = run.model.for.parameters(variable.parameters = params.manual,
                                      end.year = 2041,
                                      location=sim.mcmc$location)

simplot(sim.mcmc.projected,
        sim.manual,
        years = 1980:2040, 
        data.types = "population",
        facet.by='age')

simplot(sim.mcmc.projected,
        sim.manual,
        years=1980:2040, 
        facet.by='age', 
        #ages = MODEL.TO.SURVEILLANCE.AGE.MAPPING$`All ages`,
        data.types='incidence')

simplot(sim.mcmc.projected,
        #sim.manual,
        years=1980:2040, 
        facet.by='age', 
        ages = MODEL.TO.SURVEILLANCE.AGE.MAPPING$`All ages`,
        data.types='prevalence') + geom_vline(xintercept = 2025)

simplot(sim.mcmc.projected,
        sim.manual,
        years=1980:2030, 
        facet.by=c('age'), 
        #ages = MODEL.TO.SURVEILLANCE.AGE.MAPPING$`All ages`,
        data.types='hiv.mortality')

simplot(sim.mcmc.projected,
        sim.manual, 
        years=1980:2030, 
        facet.by=c('age',"sex"), 
        ages = "15+", 
        data.types='hiv.mortality')

simplot(sim.mcmc.projected,
        sim.manual,
        years=1980:2030, 
        facet.by=c('age'), 
        sexes = "female",
        data.types='total.mortality')

simplot(sim.mcmc.projected,
        sim.manual,
        years=1980:2020, 
        data.types='suppression', 
        proportion=T)

simplot(sim.mcmc.projected,
        sim.manual,
        years=1980:2020, 
        facet.by=c('age','sex'),
        data.types='suppression', 
        proportion=T)

simplot(sim.mcmc.projected,
        sim.manual, 
        years=1980:2020, 
        facet.by=c('age'),
        ages = MODEL.TO.SURVEILLANCE.AGE.MAPPING$`All ages`,
        data.types='suppression', 
        proportion=T)

plot.age.distribution(sim.mcmc.projected,plot.limits = c(0,200000))
plot.age.distribution(sim.manual,plot.limits = c(0,1000000))

likelihood.to.run = create.likelihood(parameters = create.model.parameters(location = "Kenya"),
                                      location="South Africa")

exp(likelihood.to.run(sim.manual) - likelihood.to.run(sim.mcmc))
exp(full.lik(sim.manual) - full.lik(sim.mcmc))
exp(pop.lik(sim.manual) - pop.lik(sim.mcmc))
exp(incidence.lik(sim.manual) - incidence.lik(sim.mcmc)) # SA: this is worse; looks basically the same? (50+)
exp(prev.lik(sim.manual) - prev.lik(sim.mcmc))
exp(aware.lik(sim.manual) - aware.lik(sim.mcmc))
exp(eng.lik(sim.manual) - eng.lik(sim.mcmc))
exp(supp.lik(sim.manual) - supp.lik(sim.mcmc)) # Kenya: this is worse; looks the exact same? (tiniest difference)
exp(hiv.mortality.lik(sim.manual) - hiv.mortality.lik(sim.mcmc))
exp(aware.trend.lik(sim.manual) - aware.trend.lik(sim.mcmc))
exp(total.mortality.lik(sim.manual) - total.mortality.lik(sim.mcmc)) # Kenya: this is worse; makes sense ; SA: worse and makes sense 



### 1/27 

simplot(simset.old@simulations[[simset.old@n.sim]],
        simset.new@simulations[[simset.new@n.sim]],
        years = 1980:2030, 
        show.individual.sims = F)

simplot(simset.old@simulations[[simset.old@n.sim]],
        simset.new@simulations[[simset.new@n.sim]],
        years = 1980:2030, 
        data.types = "population",
        show.individual.sims = F)

simplot(simset.old@simulations[[simset.old@n.sim]],
        simset.new@simulations[[simset.new@n.sim]],
        years = 1980:2030, 
        data.types = "population",
        facet.by='age', 
        show.individual.sims = F)

simplot(simset.old@simulations[[simset.old@n.sim]],
        simset.new@simulations[[simset.new@n.sim]],
        years=1980:2030, 
        facet.by='age', 
        #ages = MODEL.TO.SURVEILLANCE.AGE.MAPPING$`All ages`,
        data.types='incidence', 
        show.individual.sims = F)

simplot(simset.old@simulations[[simset.old@n.sim]],
        simset.new@simulations[[simset.new@n.sim]],
        years=1980:2030, 
        facet.by='age', 
        #ages = MODEL.TO.SURVEILLANCE.AGE.MAPPING$`All ages`,
        data.types='prevalence', 
        show.individual.sims = F)

simplot(simset.old@simulations[[simset.old@n.sim]],
        simset.new@simulations[[simset.new@n.sim]],
        years=1980:2030, 
        facet.by=c('age'), 
        #ages = MODEL.TO.SURVEILLANCE.AGE.MAPPING$`All ages`,
        data.types='hiv.mortality', 
        show.individual.sims = F)

simplot(simset.old@simulations[[simset.old@n.sim]],
        simset.new@simulations[[simset.new@n.sim]],
        years=1980:2030, 
        facet.by=c('age',"sex"), 
        ages = "15+", 
        data.types='incidence', 
        show.individual.sims = F)

simplot(simset.old@simulations[[simset.old@n.sim]],
        simset.new@simulations[[simset.new@n.sim]], 
        years=1980:2030, 
        facet.by=c('age',"sex"), 
        ages = "15+", 
        data.types='prevalence', 
        show.individual.sims = F)

simplot(simset.old@simulations[[simset.old@n.sim]],
        simset.new@simulations[[simset.new@n.sim]],
        years=1980:2030, 
        data.types='total.mortality', 
        show.individual.sims = F)

simplot(simset.old@simulations[[simset.old@n.sim]],
        simset.new@simulations[[simset.new@n.sim]],
        years=1980:2030, 
        facet.by=c('age'), 
        #sexes = "male",
        data.types='total.mortality', 
        show.individual.sims = F)

simplot(simset.old@simulations[[simset.old@n.sim]],
        simset.new@simulations[[simset.new@n.sim]], 
        years=1980:2020, 
        data.types='engagement', 
        proportion=T,
        show.individual.sims = F)

simplot(simset.old@simulations[[simset.old@n.sim]],
        simset.new@simulations[[simset.new@n.sim]], 
        years=1980:2020, 
        facet.by=c('age','sex'), 
        data.types='engagement', 
        proportion=T,
        show.individual.sims = F)

simplot(simset.old@simulations[[simset.old@n.sim]],
        simset.new@simulations[[simset.new@n.sim]],
        years=1980:2020, 
        data.types='suppression', 
        proportion=T,
        show.individual.sims = F)

simplot(simset.old@simulations[[simset.old@n.sim]],
        simset.new@simulations[[simset.new@n.sim]], 
        years=1980:2020, 
        facet.by=c('age','sex'),
        data.types='suppression', 
        proportion=T,
        show.individual.sims = F)

simplot(simset.old@simulations[[simset.old@n.sim]],
        simset.new@simulations[[simset.new@n.sim]], 
        years=1980:2020, 
        facet.by=c('age'),
        ages = MODEL.TO.SURVEILLANCE.AGE.MAPPING$`All ages`,
        data.types='suppression', 
        proportion=T,
        show.individual.sims = F)


### 1/23
load("mcmc_runs/simset_south_africa_2025-01-21.Rdata")
#source("calibration/likelihood/individual_likelihoods.R")
params.last = simset@parameters[simset@n.sim,]
cbind(params.last)

sim.mcmc = simset@simulations[[simset@n.sim]]
sim.manual = run.model.for.parameters(location="South Africa",variable.parameters = params.last)

params.last["age.0.to.14.hiv.mortality.multiplier.0"]  = 2 # 0.557803001
params.last["age.0.to.14.hiv.mortality.multiplier.1"]  = 2 # 0.591995125
params.last["age.0.to.14.hiv.mortality.multiplier.2"]  = 2 # 0.332626264

params.last["birth.transmission.risk.0"]  = 0.65 # 0.462920104 (1990)
#params.last["birth.transmission.risk.1"]  = 0.055116111 # 0.055116111 (2020)

# params.last["age.15.to.24.hiv.mortality.multiplier.0"]  = 0.5 # 0.827507577
# params.last["age.15.to.24.hiv.mortality.multiplier.1"]  = 0.1 # 0.250008119
# params.last["age.15.to.24.hiv.mortality.multiplier.2"]  = 0.25 # 0.472140556
  
#params.last["over.65.mortality.intercept.multiplier"]  = 2 # 1.646009530
#params.last["over.65.mortality.slope.multiplier"]  = 1 # 0.982697569

sim.manual = run.model.for.parameters(location="South Africa",variable.parameters = params.last)

simplot(sim.mcmc,sim.manual,
        years=1980:2030, 
        facet.by='age', 
        data.types='incidence', 
        show.individual.sims = F)

simplot(sim.mcmc,sim.manual,
        years=1980:2030, 
        facet.by='age', 
        data.types='prevalence', 
        show.individual.sims = F)

simplot(sim.mcmc,sim.manual,
        years=1980:2030, 
        facet.by='age', 
        data.types='hiv.mortality', 
        show.individual.sims = F)

simplot(sim.mcmc,sim.manual,
        years=1980:2030, 
        facet.by='age', 
        # sexes = "female",
        data.types='population', 
        show.individual.sims = F)

simplot(sim.mcmc,sim.manual,
        years=1980:2030, 
        facet.by='age', 
        #sexes = "male",
        data.types='total.mortality', 
        show.individual.sims = F)

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



### 1/17 
params.bad = simset.kenya@parameters[simset.kenya@n.sim,]
params.bad.new = params.bad
params.bad.new["over.50.aging.factor"] = 1

cbind(params.bad,params.bad.new)

sim.test.kenya = run.model.for.parameters(location="Kenya",variable.parameters = params.bad.new)

simplot(sim.test.kenya,simset.kenya@simulations[[simset.kenya@n.sim]],
        years=1980:2030, 
        facet.by='age', 
        ages = MODEL.TO.SURVEILLANCE.AGE.MAPPING$`All ages`,
        data.types='prevalence', 
        show.individual.sims = F)




### 1/14 scratch 

load("~/Library/CloudStorage/Dropbox/Documents_local/Hopkins/PhD/Dissertation/ABM/kenya_hivncd/cached/all.results_2023-04-12.Rdata")
simset.old = simset.list.full$no.int

sim.old = simset.list.full$no.int@simulations[[simset.list.full$no.int@n.sim]]
sim.old$location = "Kenya"
full.results.array.old = full.results.array

full.results.array.old[,,,,c(1:101),] = full.results.array.old[,,,,c(102),] # if I do this, the plot is similarly weird

load("~/Library/CloudStorage/Dropbox/Documents_local/Hopkins/SOM_Job/3_Aging_multimorbidity/gmha/cached/all.results_kenya_2025-01-13.Rdata")
sim.new = simset.list.full$no.int@simulations[[simset.list.full$no.int@n.sim]]
full.results.array.new = full.results.array

full.results.array.new.short = generate.full.results.array(simset.list = simset.list.full) # thinned to two sims

simplot(sim.old, sim.new,
        years=1980:2040, 
        facet.by='age', 
        ages = MODEL.TO.SURVEILLANCE.AGE.MAPPING$`All ages`,
        data.types='prevalence', 
        show.individual.sims = F) + geom_vline(xintercept = 2025, linetype="dashed",alpha=0.5) 

generate.age.distribution(full.results.array.new.short, 
                          outcome="prevalence", 
                          intervention.1 = "no.int",year.1="2025",
                          intervention.2 = "no.int",year.2="2040",
                          intervention.3 = "no.int",year.3="2040",
                          percent=F,
                          sexes = c("female","male"),
                          plot.limits=c(0,200000)) +  # 200000 for kenya; 1000000 for south africa 
    scale_fill_manual(labels = c("no.int/2025" = "2025",
                                 "no.int/2040" = "Status quo, 2040",
                                 "no.int/2040" = "Full intervention, 2040"), 
                      values=alpha(c("no.int/2025" = pal[1],
                                     "no.int/2040" = pal[2],
                                     "no.int/2040" = pal[3]),alpha), # change legend and color scheme  
                      name=NULL) +
    theme(text = element_text(size = 20),
          axis.title.y = element_text(colour = "grey37"))+
    labs(title = NULL,subtitle = NULL) +
    ylab(label = "Number of people living with HIV")+
    guides(x =  guide_axis(angle = 45))


### 1/10 scratch 

params = simset@parameters[simset@n.sim,] 

### 1/9 scratch 

sim.last = simset@simulations[[simset@n.sim]]

total.mort.sim = extract.data(sim.last,
                              data.type = "total.mortality",
                              years = c(1980:2023),
                              keep.dimensions = c("year","age"))
non.hiv.mort.sim = extract.data(sim.last,
                              data.type = "non.hiv.mortality",
                              years = c(1980:2023),
                              keep.dimensions = c("year","age"))

hiv.mort.sim = extract.data(sim.last,
                                data.type = "hiv.mortality",
                                years = c(1980:2023),
                                keep.dimensions = c("year","age"))

total.mort.data = get.surveillance.data(DATA.MANAGER,
                                        data.type = "total.mortality",
                                        years = c(1980:2023),
                                        locations = "Kenya",
                                        keep.dimensions = c("year","age","location"))
pop.data = get.surveillance.data(DATA.MANAGER,
                                 data.type = "population",
                                 years = c(1980:2023),
                                 locations = "Kenya",
                                 keep.dimensions = c("year","age","location"))

dim(total.mort.data) = dim(total.mort.sim)
dim(pop.data) = dim(total.mort.sim)
dimnames(total.mort.data) = dimnames(total.mort.sim)
dimnames(pop.data) = dimnames(total.mort.sim)

diff = total.mort.data - total.mort.sim

diff.of.mort.data = (diff/total.mort.data)*100
diff.of.pop.data = (diff/pop.data)*100

plot.df.non.hiv.mort = melt(non.hiv.mort.sim)
plot.df.hiv.mort = melt(hiv.mort.sim)

## TO SHOW PARASTU 
simplot(sim.last,
    years=1980:2030, 
    facet.by=c('age'), 
    data.types='total.mortality', 
    show.individual.sims = F) + 
    geom_line(data = plot.df.non.hiv.mort, aes(x = year, y = value)) +
    #geom_line(data = plot.df.hiv.mort, aes(x = year, y = value)) +
    facet_wrap(~age, scales = "free_y") + 
    ylim(0,NA)

simplot(simset, 
        years=1980:2030, 
        facet.by=c('age'), 
        ages = MODEL.TO.SURVEILLANCE.AGE.MAPPING$`All ages`,
        data.types='hiv.mortality', 
        show.individual.sims = F)

simplot(simset, 
        years=1980:2030, 
        facet.by=c('age'), 
        data.types='hiv.mortality', 
        show.individual.sims = F)

simplot(simset, 
        years = 1980:2030, 
        data.types = "population",
        facet.by='age', 
        show.individual.sims = F)

which(sim.last$parameters$time.varying.parameters$NON.HIV.MORTALITY.RATES$times==2005) # 56
which(sim.last$parameters$time.varying.parameters$NON.HIV.MORTALITY.RATES$times==2010) # 61
rates.2005 = apply(sim.last$parameters$time.varying.parameters$NON.HIV.MORTALITY.RATES$values[[55]],c("age"),mean)
rates.2010 = apply(sim.last$parameters$time.varying.parameters$NON.HIV.MORTALITY.RATES$values[[61]],c("age"),mean)

rates.2010/rates.2005

hiv.rates.2005 = apply(sim.last$parameters$time.varying.parameters$HIV.MORTALITY.RATES$values[[2]][,,,c("undiagnosed","diagnosed_unengaged","engaged_unsuppressed")],c("age"),mean) # 2005
hiv.rates.2020 = apply(sim.last$parameters$time.varying.parameters$HIV.MORTALITY.RATES$values[[3]][,,,c("undiagnosed","diagnosed_unengaged","engaged_unsuppressed")],c("age"),mean) # 2020

hiv.rates.2020/hiv.rates.2005



### 1/8 scratch 

lik = create.likelihood.for.data.type(data.type = "total.mortality",
                                      data.manager=DATA.MANAGER,
                                      years=1980:2020,
                                      location="Kenya",
                                      parameters=BASE.PARAMETERS.KENYA,
                                      denominator.data.type=NULL, 
                                      obs.is.proportion=F,
                                      weight=1/1000, # *WEIGHTS.BY.YEAR
                                      obs.correlation=0.5, 
                                      correlation.structure="auto.regressive",
                                      calculate.sds.from.ci=F,
                                      use.total=F,
                                      use.sex=F,
                                      use.age=F,
                                      use.age.sex=T) 

exp(lik(simset.new@simulations[[simset.new@n.sim]]) - 
        lik(simset.old@simulations[[simset.old@n.sim]]))

lik(simset.new@simulations[[simset.new@n.sim]],debug = T)

### old scratch 

sim.mcmc = simset.test@simulations[[simset.test@n.sim]]


simplot(sim.kenya,
        sim.mcmc,
        years=c(1970:2020),
        data.types = "population", facet.by = 'age')


simplot(sim.kenya,
        sim.mcmc,
        years=c(1970:2020),
        data.types = "incidence", facet.by = 'age')

simplot(sim.kenya,
        sim.mcmc,
        years=c(1970:2020),
        data.types = "prevalence", facet.by = 'age')

simplot(sim.kenya,
        sim.mcmc,
        years=c(1970:2020),
        data.types = "suppression", facet.by = c('age','sex'),
        proportion = T)


BASE.PARAMETERS=create.model.parameters(location = "Kenya")
likelihood = create.likelihood(parameters = BASE.PARAMETERS,
                               location = "Kenya")

incidence.lik = create.individual.likelihood(data.type = "incidence",
                                             parameters = BASE.PARAMETERS,
                                             location = "Kenya")
prev.lik = create.individual.likelihood(data.type = "prevalence",
                                        parameters = BASE.PARAMETERS,
                                        location = "Kenya")
pop.lik = create.individual.likelihood(data.type = "population",
                                        parameters = BASE.PARAMETERS,
                                       location = "Kenya")
aware.lik = create.individual.likelihood(data.type = "awareness",
                                       parameters = BASE.PARAMETERS,
                                       location = "Kenya")
eng.lik = create.individual.likelihood(data.type = "engagement",
                                       parameters = BASE.PARAMETERS,
                                       location = "Kenya")
supp.lik = create.individual.likelihood(data.type = "suppression",
                                       parameters = BASE.PARAMETERS,
                                       location = "Kenya")
hiv.mortality.lik = create.individual.likelihood(data.type = "hiv.mortality",
                                       parameters = BASE.PARAMETERS,
                                       location = "Kenya")
aware.trend.lik = create.individual.likelihood(data.type = "awareness.trend",
                                                 parameters = BASE.PARAMETERS,
                                               location = "Kenya")

exp(likelihood(sim.mcmc) - likelihood(sim.kenya))
exp(incidence.lik(sim.mcmc) - incidence.lik(sim.kenya))
exp(prev.lik(sim.mcmc) - prev.lik(sim.kenya))
exp(pop.lik(sim.mcmc) - pop.lik(sim.kenya))
exp(aware.lik(sim.mcmc) - aware.lik(sim.kenya))
exp(eng.lik(sim.mcmc) - eng.lik(sim.kenya))
exp(supp.lik(sim.mcmc) - supp.lik(sim.kenya))
exp(hiv.mortality.lik(sim.mcmc) - hiv.mortality.lik(sim.kenya))
exp(aware.trend.lik(sim.mcmc) - aware.trend.lik(sim.kenya))



