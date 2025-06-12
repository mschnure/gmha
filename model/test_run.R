source('model/run_systematic.R')

variable.parameters.france = get.default.parameters(location = "France")
sim.france = run.model.for.parameters(location="France",variable.parameters = variable.parameters.france)

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


simplot(sim.france,
        #sim.unaids.remainder,
        #sim.non.unaids.remainder,
        years=c(1970:2030),
        data.types = c("incidence","prevalence"))

simplot(sim.france,
        #sim.r1.low,
        # sim.r1.lower.middle,
        # sim.r1.upper.middle,
        # sim.r1.high,
        # sim.unaids.remainder,
        # sim.non.unaids.remainder,
        years=c(1980:2020),
        data.types = c("incidence"),
        facet.by = 'age')

simplot(sim.france,
        #sim.unaids.remainder,
        #sim.non.unaids.remainder,
        years=c(1980:2020),
        data.types = c("prevalence"),
        facet.by = 'age')

simplot(sim.france,
        years=c(1970:2020),
        data.types = "population")
 
simplot(sim.france,
        #sim.r1.low,
        # sim.r1.lower.middle,
        # sim.r1.upper.middle,
        # sim.r1.high,
        # sim.unaids.remainder,
        # sim.non.unaids.remainder,
        years=c(1970:2020),
        data.types = "population", facet.by = 'age')

simplot(sim.france,
        #sim.unaids.remainder,
        #sim.non.unaids.remainder,
        years=c(1970:2020),
        data.types = "total.mortality", facet.by = 'age')

simplot(sim.france,
        #sim.unaids.remainder,
        #sim.non.unaids.remainder,
        years=2010:2030, 
        data.types=c('awareness',"engagement","suppression"), 
        proportion=T)

simplot(sim.france,
        #sim.r1.low,
        # sim.r1.lower.middle,
        # sim.r1.upper.middle,
        # sim.r1.high,
        # sim.unaids.remainder,
        # sim.non.unaids.remainder,
        years=2010:2030, 
        data.types=c('awareness',"engagement","suppression"), 
        facet.by=c('age','sex'), 
        proportion=T)

