source('model/run_systematic.R')

variable.parameters.france = get.default.parameters(location = "France")
variable.parameters.kenya = get.default.parameters(location = "Kenya")
variable.parameters.france["trate.0"] # 0.5, 1990
variable.parameters.france["trate.1"] = 0.12# 0.1, 1997
variable.parameters.france["trate.2"] = 0.12 # 0.1, 2008
variable.parameters.france["trate.3"] = 0.15 # 0.1, 2018
variable.parameters.france["trate.4"] = 0.15 # 0.1, 2030? 
variable.parameters.france["age.20.to.29.transmission.multiplier.2"] = 0.85
variable.parameters.france["age.20.to.29.transmission.multiplier.3"] = 0.5
variable.parameters.france["age.40.to.49.transmission.multiplier.0"] = 1.25

#variable.parameters.france["age.15.to.19.transmission.multiplier.2"] = 0.25 # 1, 2008 

sim.france = run.model.for.parameters(location="France",variable.parameters = variable.parameters.france)
sim.kenya = run.model.for.parameters(location="Kenya",variable.parameters = variable.parameters.kenya)


simplot(sim.france,
        years=c(1970:2030),
        data.types = c("incidence","prevalence"))

simplot(sim.france,
        years=c(1980:2020),
        data.types = c("incidence"),
        facet.by = 'age')

simplot(sim.france,
        years=c(1980:2020),
        data.types = c("prevalence"),
        facet.by = 'age')

simplot(sim.france,
        years=c(1970:2020),
        data.types = "population")
 
simplot(sim.france,
        years=c(1970:2020),
        #sexes = "male",
        data.types = "population", facet.by = 'age')

simplot(sim.france,
        years=c(1970:2020),
        data.types = "total.mortality", facet.by = 'age')

simplot(sim.france, 
        years=1980:2030, 
        data.types='hiv.mortality')

simplot(sim.france, 
        years=1980:2030, 
        facet.by=c('age'), 
        #ages = MODEL.TO.SURVEILLANCE.AGE.MAPPING$`All ages`,
        data.types='hiv.mortality')

simplot(sim.france,
        #sim.kenya,
        #sim.unaids.remainder,
        #sim.non.unaids.remainder,
        years=1980:2030, 
        data.types=c('awareness',"engagement","suppression"), 
        proportion=T)

simplot(sim.france,
        years=1980:2030, 
        data.types=c('awareness',"engagement","suppression"), 
        facet.by=c('age','sex'), 
        proportion=T)

simplot(sim.france, 
        years=1980:2030, 
        facet.by=c('age',"sex"), 
        ages = "15+", 
        data.types='incidence')

simplot(sim.france, 
        years=1980:2030, 
        facet.by=c('age',"sex"), 
        ages = "15+", 
        data.types='prevalence')

simplot(sim.france, 
        years=1980:2030, 
        facet.by=c('age',"sex"), 
        ages = "15+", 
        data.types='hiv.mortality')


# variable.parameters.r1.low=get.default.parameters(location = "r1.low")
# variable.parameters.r1.lower.middle=get.default.parameters(location = "r1.lower.middle")
# variable.parameters.r1.upper.middle=get.default.parameters(location = "r1.upper.middle")
# variable.parameters.r1.high=get.default.parameters(location = "r1.high")
# variable.parameters.unaids.remainder=get.default.parameters(location = "unaids.remainder")
# variable.parameters.non.unaids.remainder=get.default.parameters(location = "non.unaids.remainder")

# sim.r1.low = run.model.for.parameters(location="r1.low",
#                                       variable.parameters = variable.parameters.r1.low)
# sim.r1.lower.middle = run.model.for.parameters(location="r1.lower.middle",
#                                       variable.parameters = variable.parameters.r1.lower.middle)
# sim.r1.upper.middle = run.model.for.parameters(location="r1.upper.middle",
#                                       variable.parameters = variable.parameters.r1.upper.middle)
# sim.r1.high = run.model.for.parameters(location="r1.high",
#                                       variable.parameters = variable.parameters.r1.high)
# sim.unaids.remainder = run.model.for.parameters(location="unaids.remainder",
#                                                 variable.parameters = variable.parameters.unaids.remainder)
# sim.non.unaids.remainder = run.model.for.parameters(location="non.unaids.remainder",
#                                                 variable.parameters = variable.parameters.non.unaids.remainder)