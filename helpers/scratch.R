### 1/10 scratch 

params = simset@parameters[simset@n.sim,] 

### 1/9 scratch 

sim.last = simset.test@simulations[[simset.test@n.sim]]

total.mort.sim = extract.data(sim.last,
                              data.type = "total.mortality",
                              years = c(1980:2023),
                              keep.dimensions = c("year","age"))
non.hiv.mort.sim = extract.data(sim.last,
                              data.type = "non.hiv.mortality",
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

simplot(sim.last,
    years=1980:2030, 
    facet.by=c('age'), 
    data.types='total.mortality', 
    show.individual.sims = F) + 
    geom_line(data = plot.df.non.hiv.mort, aes(x = year, y = value)) +
    facet_wrap(~age, scales = "free_y") + 
    ylim(0,NA)


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



