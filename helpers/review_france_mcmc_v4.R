source("model/run_systematic.R")

source('calibration/likelihood/likelihood.R')
source('calibration/prior_distributions/france_prior.R')
source("calibration/likelihood/individual_likelihoods.R") # make sure location set to France

load("calibration/starting_values/2025_02_26_france_start_values.Rdata")
params = params.start.values

sim = run.model.for.parameters(variable.parameters = params,
                                      end.year = 2040,
                                      location = "France")

simplot(sim,
        years = 1980:2030)

simplot(sim,
        years = 1980:2030,
        data.types = "population",
        facet.by='age')

simplot(sim,
        years=1980:2030, 
        facet.by='age', 
        data.types='incidence')

simplot(sim,
        years=1980:2030, 
        facet.by='age', 
        data.types='prevalence')

likelihood.to.run = create.likelihood(parameters = create.model.parameters(location = "France"),
                                      location="France")

(likelihood.to.run(sim))

(full.lik(sim))
(pop.lik(sim))
(incidence.lik(sim))
(prev.lik(sim))
(aware.lik(sim))
(eng.lik(sim))
(supp.lik(sim))
(hiv.mortality.lik(sim))
(aware.trend.lik(sim))
(total.mortality.lik(sim))
