source('model/run_systematic.R')

variable.parameters.cambodia = get.default.parameters(location = "Cambodia")
sim.cambodia = run.model.for.parameters(location="Cambodia",variable.parameters = variable.parameters.cambodia)

simplot(sim.cambodia,
        years=c(1970:2030),
        data.types = c("incidence","prevalence"))

simplot(sim.cambodia,
        years=c(1980:2020),
        data.types = c("incidence"),
        facet.by = 'age')

simplot(sim.cambodia,
        years=c(1980:2020),
        data.types = c("prevalence"),
        facet.by = 'age')

simplot(sim.cambodia,
        years=c(1970:2020),
        data.types = "population")
 
simplot(sim.cambodia,
        years=c(1970:2020),
        #sexes = "male",
        data.types = "population", facet.by = 'age')

simplot(sim.cambodia,
        years=c(1970:2020),
        data.types = "total.mortality", facet.by = 'age')

simplot(sim.cambodia, 
        years=1980:2030, 
        data.types='hiv.mortality')

simplot(sim.cambodia, 
        years=1980:2030, 
        facet.by=c('age'), 
        #ages = MODEL.TO.SURVEILLANCE.AGE.MAPPING$`All ages`,
        data.types='hiv.mortality')

simplot(sim.cambodia,
        years=1980:2030, 
        data.types=c('awareness',"engagement","suppression"), 
        proportion=T)

simplot(sim.cambodia,
        years=1980:2030, 
        data.types=c('awareness',"engagement","suppression"), 
        facet.by=c('age','sex'), 
        proportion=T)

simplot(sim.cambodia, 
        years=1980:2030, 
        facet.by=c('age',"sex"), 
        ages = "15+", 
        data.types='incidence')

simplot(sim.cambodia, 
        years=1980:2030, 
        facet.by=c('age',"sex"), 
        ages = "15+", 
        data.types='prevalence')

simplot(sim.cambodia, 
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