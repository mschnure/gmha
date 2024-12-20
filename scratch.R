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



