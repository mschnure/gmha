source('model/run_systematic.R')

variable.parameters.unaids.remainder=get.default.parameters(location = "unaids.remainder")
variable.parameters.non.unaids.remainder=get.default.parameters(location = "non.unaids.remainder")
#variable.parameters.nigeria=get.default.parameters(location = "Nigeria")
# variable.parameters.malawi=get.default.parameters(location = "Malawi")
# variable.parameters.zambia=get.default.parameters(location = "Zambia")
# variable.parameters.zimbabwe=get.default.parameters(location = "Zimbabwe")
# variable.parameters.uganda=get.default.parameters(location = "Uganda")
# variable.parameters.tanzania=get.default.parameters(location = "Tanzania")
# variable.parameters.kenya=get.default.parameters(location = "Kenya")
# variable.parameters.south.africa=get.default.parameters(location = "South Africa")
# variable.parameters.france=get.default.parameters(location = "France")
# variable.parameters.mozambique=get.default.parameters(location = "Mozambique")
#variable.parameters.thailand=get.default.parameters(location = "Thailand")
#variable.parameters.cambodia=get.default.parameters(location = "Cambodia")

sim.unaids.remainder = run.model.for.parameters(location="unaids.remainder",
                                                variable.parameters = variable.parameters.unaids.remainder)
sim.non.unaids.remainder = run.model.for.parameters(location="non.unaids.remainder",
                                                variable.parameters = variable.parameters.non.unaids.remainder)
# sim.nigeria = run.model.for.parameters(location="Nigeria",variable.parameters = variable.parameters.nigeria)
# sim.malawi = run.model.for.parameters(location="Malawi",variable.parameters = variable.parameters.malawi)
# sim.zambia = run.model.for.parameters(location="Zambia",variable.parameters = variable.parameters.zambia)
# sim.zimbabwe = run.model.for.parameters(location="Zimbabwe",variable.parameters = variable.parameters.zimbabwe)
#sim.uganda = run.model.for.parameters(location="Uganda",variable.parameters = variable.parameters.uganda)
#sim.tanzania = run.model.for.parameters(location="Tanzania",variable.parameters = variable.parameters.tanzania)
#sim.mozambique = run.model.for.parameters(location="Mozambique",variable.parameters = variable.parameters.mozambique)
#sim.kenya = run.model.for.parameters(location="Kenya",variable.parameters = variable.parameters.kenya)
#sim.south.africa = run.model.for.parameters(location="South Africa",variable.parameters = variable.parameters.south.africa)
#sim.france = run.model.for.parameters(location="France",variable.parameters = variable.parameters.france)
#sim.thailand = run.model.for.parameters(location="Thailand",variable.parameters = variable.parameters.thailand)
#sim.cambodia = run.model.for.parameters(location="Cambodia",variable.parameters = variable.parameters.cambodia)

simplot(#sim.unaids.remainder,
        sim.non.unaids.remainder,
        #sim.nigeria,
        #sim.malawi,
        #sim.zambia,
        #sim.zimbabwe,
        #sim.uganda,
        #sim.tanzania,
        #sim.mozambique,
        #sim.kenya,
        #sim.south.africa,
        #sim.france,
        #sim.thailand,
        #sim.cambodia,
        years=c(1970:2030),
        data.types = c("incidence","prevalence"))

simplot(#sim.unaids.remainder,
        sim.non.unaids.remainder,
        #sim.nigeria,
        #sim.malawi,
        #sim.zambia,
        #sim.zimbabwe,
        #sim.uganda,
        #sim.tanzania,
        #sim.mozambique,
        #sim.kenya,
        #sim.south.africa,
        #sim.cambodia,
        #sim.thailand,
        #sim.france,
        years=c(1980:2020),
        data.types = c("incidence"),
        facet.by = 'age')

simplot(sim.unaids.remainder,
        #sim.non.unaids.remainder,
        #sim.nigeria,
        #sim.malawi,
        #sim.zambia,
        #sim.zimbabwe,
        #sim.uganda,
        #sim.tanzania,
        #sim.mozambique,
        #sim.kenya,
        #sim.south.africa,
        #sim.cambodia,
        #sim.thailand,
        #sim.netherlands,
        #sim.france,
        years=c(1980:2020),
        data.types = c("prevalence"),
        facet.by = 'age')

simplot(sim.nigeria,
        #sim.malawi,
        #sim.zambia,
        #sim.zimbabwe,
        #sim.uganda,
        #sim.tanzania,
        #sim.mozambique,
        #sim.kenya,
        #sim.south.africa,
        #sim.cambodia,
        #sim.thailand,
        #sim.france,
        years=c(1970:2020),
        data.types = "population")
 
simplot(#sim.unaids.remainder,
        sim.non.unaids.remainder,
        #sim.nigeria,
        #sim.malawi,
        #sim.zambia,
        #sim.zimbabwe,
        #sim.uganda,
        #sim.tanzania,
        #sim.mozambique,
        #sim.kenya,
        #sim.south.africa,
        #sim.cambodia,
        #sim.thailand,
        #sim.france,
        years=c(1970:2020),
        data.types = "population", facet.by = 'age')

simplot(sim.nigeria,
        #sim.malawi,
        #sim.zambia,
        #sim.zimbabwe,
        #sim.uganda,
        #sim.tanzania,
        #sim.mozambique,
        #sim.kenya,
        #sim.south.africa,
        #sim.cambodia,
        #sim.thailand,
        #sim.france,
        years=c(1970:2020),
        data.types = "total.mortality", facet.by = 'age')

simplot(sim.nigeria,
        #sim.malawi,
        #sim.zambia,
        #sim.zimbabwe,
        #sim.uganda,
        #sim.tanzania,
        years=2010:2030, 
        data.types=c('awareness',"engagement","suppression"), 
        proportion=T)

simplot(sim.nigeria,
        #sim.malawi,
        #sim.zambia,
        #sim.zimbabwe,
        #sim.uganda,
        #sim.tanzania,
        years=2010:2030, 
        data.types=c('awareness',"engagement","suppression"), 
        facet.by=c('age','sex'), 
        proportion=T)

