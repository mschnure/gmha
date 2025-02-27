source('model/run_systematic.R')

#LOCATION = "South Africa"
variable.parameters.kenya=get.default.parameters(location = "Kenya")
variable.parameters.south.africa=get.default.parameters(location = "South Africa")
variable.parameters.france=get.default.parameters(location = "France")
#variable.parameters.thailand=get.default.parameters(location = "Thailand")
#variable.parameters.cambodia=get.default.parameters(location = "Cambodia")

#set.seed(1234)
sim.france = run.model.for.parameters(location="France",variable.parameters = variable.parameters.france)
#sim.kenya = run.model.for.parameters(location="Kenya",variable.parameters = variable.parameters.kenya)
#sim.south.africa = run.model.for.parameters(location="South Africa",variable.parameters = variable.parameters.south.africa)
#sim.thailand = run.model.for.parameters(location="Thailand",variable.parameters = variable.parameters.thailand)
#sim.cambodia = run.model.for.parameters(location="Cambodia",variable.parameters = variable.parameters.cambodia)

simplot(#sim.kenya,
        #sim.south.africa,
        sim.france,
        #sim.thailand,
        #sim.cambodia,
        years=c(1970:2030),
        data.types = c("incidence","prevalence"))

simplot(#sim.kenya,
        #sim.south.africa,
        #sim.cambodia,
        #sim.thailand,
        sim.france,
        years=c(1980:2020),
        data.types = c("incidence"),
        facet.by = 'age')

simplot(sim.kenya,
        #sim.south.africa,
        #sim.cambodia,
        #sim.thailand,
        #sim.netherlands,
        #sim.france,
        years=c(1980:2020),
        data.types = c("prevalence"),
        facet.by = 'age')

simplot(#sim.kenya,
        #sim.south.africa,
        #sim.cambodia,
        #sim.thailand,
        sim.france,
        years=c(1970:2020),
        data.types = "population")
 
simplot(#sim.kenya,
        #sim.south.africa,
        #sim.cambodia,
        #sim.thailand,
        sim.france,
        years=c(1970:2020),
        data.types = "population", facet.by = 'age')

simplot(sim.kenya,
        #sim.south.africa,
        #sim.cambodia,
        #sim.thailand,
        #sim.france,
        years=c(1970:2020),
        data.types = "total.mortality", facet.by = 'age')

simplot(#sim.kenya,
        sim.south.africa, 
        years=1980:2020, 
        data.types='engagement', 
        proportion=T)

simplot(sim.kenya,
        #sim.south.africa, 
        years=1980:2020, 
        data.types='suppression', 
        proportion=T)
