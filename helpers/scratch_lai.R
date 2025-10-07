source('model/run_systematic.R')

load("calibration/starting_values/2025_08_11_south_africa_start_values.Rdata")
variable.parameters = params.start.values
variable.parameters['age.0.14.engagement.multiplier'] = 0.13
variable.parameters['log.OR.0.14.engagement.slope'] = 0.25
variable.parameters['age.0.14.suppression.multiplier'] = 0.15 
variable.parameters['log.OR.0.14.suppression.slope'] = 0.25

#variable.parameters["unsuppressed.disengagement.rates"] = variable.parameters["suppressed.disengagement.rates"] 
# this was the problem - much higher disengagement from unsuppressed

sim = run.model.for.parameters(location="South Africa",variable.parameters = variable.parameters)
#sim.orig = sim

simplot(sim.orig,
        sim,
        years=1980:2030, 
        data.types=c('awareness',"engagement","suppression"), 
        facet.by=c('age'), 
        proportion=T)




###



#eng.rates.no.mult[[30]] # the rates are the same? (saved these within the browser)
#eng.rates.with.mult[[30]]

eng.sim.mult = extract.total.engaged(sim,ages = c("0-4","5-9","10-14"))
eng.sim.no.mult = extract.total.engaged(sim.orig,ages = c("0-4","5-9","10-14"))

round(cbind(eng.sim.no.mult,eng.sim.mult),2)

annual.eng.mult = sim$engagement["2030",c("0-4","5-9","10-14"),,]
annual.eng.no.mult = sim.orig$engagement["2030",c("0-4","5-9","10-14"),,]

annual.eng.mult # this one should be lower
annual.eng.no.mult 

simplot(sim.orig,
        sim,
        years=1980:2030, 
        data.types=c('awareness',"engagement","suppression"), 
        facet.by=c('age'), 
        proportion=T)




###




source('model/run_systematic.R')

#variable.parameters = get.default.parameters(location = "South Africa")
load("calibration/starting_values/2025_08_11_south_africa_start_values.Rdata")
variable.parameters = params.start.values

sim = run.model.for.parameters(location="South Africa",variable.parameters = variable.parameters)

sim.numerator = extract.data(sim = sim,
                             data.type = "suppression",
                             years= 2010:2020,
                             keep.dimensions = c("year"))

sim.denominator = extract.data(sim = sim,
                               data.type = "awareness",
                               years= 2010:2020,
                               keep.dimensions = c("year"))

sim.suppression = sim.numerator/sim.denominator # suppressed/aware
DATA.MANAGER$suppression$year.location[,"South Africa"] # suppression out of awareness - correct 
DATA.MANAGER$engagement$year.location[,"South Africa"] # this is out of awareness - correct 

simplot(sim,   
        years=1980:2030, 
        data.types=c("suppression"), 
        proportion=T) +geom_hline(yintercept = .64226759031) +geom_hline(yintercept = 0.84) + geom_vline(xintercept = 2015)
# 2015 sim value and data value 
# fixed 2015 data value: 0.5695 (old value: 0.84)





###



oral.suppression = extract.data(sim,"suppression.oral")
round(oral.suppression)
total.suppression = extract.data(sim,"suppression")
round(total.suppression)
lai.suppression = extract.data(sim,"suppression.lai")
round(lai.suppression)

aware = extract.data(sim,"awareness")

round(total.suppression/aware,3)