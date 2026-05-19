source("model/run_systematic.R")

# load("~/gmha/mcmc_runs/simset_r1.high_2026-04-13.Rdata")
# simset.old = simset

load("mcmc_runs/simset_r1.high_1_2026-04-20.Rdata")
simset.bad = simset

load("mcmc_runs/simset_r1.high_1_2026-04-23.Rdata")
simset.new = simset

load("mcmc_runs/simset_r1.high_1_2026-04-27.Rdata")
simset.new.2 = simset

load("mcmc_runs/simset_r1.high_chain1_2026-05-19.Rdata")
simset.5.19 = simset

default.params = get.default.parameters(location = "r1.high")
#default.params.low = get.default.parameters(location = "r1.low")
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

#sim.default = run.model.for.parameters(location="r1.high",variable.parameters = default.params)
#sim.default.low = run.model.for.parameters(location="r1.low",variable.parameters = default.params.low)
sim.test = run.model.for.parameters(location="r1.high",variable.parameters = variable.parameters.test)

simplot(#sim.default,
        #sim.test.old,
        #sim.test,
        simset.new.2, 
        simset.5.19,
        #sim.default.low,
        years=1980:2040, 
        facet.by='age', 
        ages = MODEL.TO.SURVEILLANCE.AGE.MAPPING$`All ages`,
        data.types='incidence') #+ geom_vline(xintercept = 1997)+ geom_vline(xintercept = 2018)
       #data.types='prevalence')#+ geom_vline(xintercept = 1997)+ geom_vline(xintercept = 2018)
       #data.types='population')+ geom_vline(xintercept = 1997)+ geom_vline(xintercept = 2018)

# time.0=1990
# time.1=1997 
# time.2=2008 
# time.3=2018 

#params.old = simset.old@parameters[490,] 
params.bad = simset.bad@parameters[490,] 
params.new = simset.new@parameters[490,] 
params.new.2 = simset.new.2@parameters[490,] 

cbind(#params.bad[grepl("transmission.multiplier|trate",names(params.bad))],
      default.params[grepl("transmission.multiplier|trate",names(default.params))],
      params.new.2[grepl("transmission.multiplier|trate",names(params.new.2))])

cbind(default.params[grepl("trate",names(default.params))],
      variable.parameters.test[grepl("trate",names(variable.parameters.test))],
      params.new.2[grepl("trate",names(params.new.2))])

cbind(default.params[grepl("hiv.specific.mortality.rates",names(default.params))],
      variable.parameters.test[grepl("hiv.specific.mortality.rates",names(variable.parameters.test))],
      params.new.2[grepl("hiv.specific.mortality.rates",names(params.new.2))])
      
