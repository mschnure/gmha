source("model/run_systematic.R")

load("mcmc_runs/simset_r1.high_1_2026-04-27.Rdata")
simset.4.27 = simset

load("mcmc_runs/simset_r1.high_chain1_2026-05-19.Rdata")
simset.5.19 = simset

load("mcmc_runs/simset_r1.high_chain1_2026-05-21.Rdata")
simset.5.21 = simset

default.params = get.default.parameters(location = "r1.high")
prior = R1.HIGH.PRIOR
prior.medians = get.medians(prior)
prior.params = default.params
prior.params[names(prior.medians)] = prior.medians

sim.prior = run.model.for.parameters(location="r1.high",variable.parameters = prior.params)

simplot(#sim.prior,
        simset.4.27,
        simset.5.21,
        #sim.last,
        years=1980:2040, 
        facet.by='age', 
        #ages = MODEL.TO.SURVEILLANCE.AGE.MAPPING$`All ages`,
        #data.types='incidence') #+ geom_vline(xintercept = 1997)+ geom_vline(xintercept = 2018)
       data.types='prevalence')#+ geom_vline(xintercept = 1997)+ geom_vline(xintercept = 2018)
       #data.types='population')+ geom_vline(xintercept = 1997)+ geom_vline(xintercept = 2018)

# time.0=1990
# time.1=1997 
# time.2=2008 
# time.3=2018 

params.4.27 = simset.4.27@parameters[490,] 
params.5.21 = simset.5.21@parameters[490,] 
sim.last = simset.5.21@simulations[[length(simset.5.21@simulations)]]

cbind(#default.params[grepl("transmission.multiplier|trate",names(default.params))],
      prior.medians[grepl("transmission.multiplier|trate",names(prior.medians))],
      #params.4.27[grepl("transmission.multiplier|trate",names(params.4.27))],
      params.5.21[grepl("transmission.multiplier|trate",names(params.5.21))])

cbind(default.params[grepl("trate",names(default.params))],
      prior.medians[grepl("trate",names(prior.medians))],
      params.4.27[grepl("trate",names(params.4.27))],
      params.5.21[grepl("trate",names(params.5.21))])

cbind(default.params[grepl("hiv.specific.mortality.rates",names(default.params))],
      prior.medians[grepl("hiv.specific.mortality.rates",names(prior.medians))],
      params.4.27[grepl("hiv.specific.mortality.rates",names(params.4.27))],
      params.5.21[grepl("hiv.specific.mortality.rates",names(params.5.21))])


if(1==2){
    source("calibration/likelihood/individual_likelihoods.R") # make sure location = "r1.high" in this file
}   


exp(full.lik(sim.last) - full.lik(sim.prior))
exp(incidence.lik(sim.last) - incidence.lik(sim.prior)) 
exp(prev.lik(sim.last) - prev.lik(sim.prior))
exp(aware.lik(sim.last) - aware.lik(sim.prior))
exp(eng.lik(sim.last) - eng.lik(sim.prior))
exp(supp.lik(sim.last) - supp.lik(sim.prior)) 
exp(pop.lik(sim.last) - pop.lik(sim.prior))
exp(hiv.mortality.lik(sim.last) - hiv.mortality.lik(sim.prior))
exp(total.mortality.lik(sim.last) - total.mortality.lik(sim.prior))       




if(1==2){
    variable.parameters.test = default.params
    variable.parameters.test["trate.0"] = 0.61#6222610 
    variable.parameters.test["trate.1"] = 0.06307164
    variable.parameters.test["trate.2"] = .08 #0.11092556 
    variable.parameters.test["trate.3"] = .08 # 0.12017985
    variable.parameters.test["trate.4"] = .08 # 0.11661092
    variable.parameters.test["age.40.to.49.transmission.multiplier.0"] = 0.8
    variable.parameters.test["age.40.to.49.transmission.multiplier.1"] = 0.8
    variable.parameters.test["age.40.to.49.transmission.multiplier.2"] = 0.8
    variable.parameters.test["age.40.to.49.transmission.multiplier.3"] = 0.8
    variable.parameters.test["age.50.and.over.transmission.multiplier.0"] = 0.5
    variable.parameters.test["age.50.and.over.transmission.multiplier.1"] = 0.5
    variable.parameters.test["age.50.and.over.transmission.multiplier.2"] = 0.5
    variable.parameters.test["age.50.and.over.transmission.multiplier.3"] = 0.5
    #variable.parameters.test["hiv.specific.mortality.rates.0"] = 0.008120033
    #variable.parameters.test["hiv.specific.mortality.rates.1"] = 0.004635217
    #variable.parameters.test["hiv.specific.mortality.rates.2"] = 0.003877367
    #variable.parameters.test["proportion.trate.change.by.3.5"] = 0.05
}



