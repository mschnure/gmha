source("source_code.R")
source('data_manager/scaling_remainder_models.R')
load('data_manager/remainder_countries.Rdata')
load('data_manager/cascade_weights.Rdata')
load('data_manager/locations_income.Rdata')

INDIVIDUAL.COUNTRIES = c("South Africa","Mozambique","Nigeria","Tanzania","Uganda","Kenya",
                         "Zambia","Zimbabwe","Malawi","France","Thailand","Cambodia") # removed Brazil, Chile, Netherlands


# Default suppression denominator = all aware PLHIV
# therefore, need to add in awareness for this age groups too - might as well add engagement 
    #  Awareness denominator = all PLHIV
    #  Default engagement denominator = all aware PLHIV 

names(DATA.MANAGER$suppression)# need to ADD year.age.location to include 0-14 
DATA.MANAGER$suppression$AGES # need to update to include 0-14 and 15-24
DATA.MANAGER$suppression$AGE.LOWERS 
DATA.MANAGER$suppression$AGE.UPPERS
dimnames(DATA.MANAGER$suppression$year.age.sex.location)[-4] # looking at dims without location for now

# this is % of people on ART who achieve viral suppression - have to fix denominator somehow 
x = read.cascade.data.files(dir = 'data_manager/data',
                            data.type="cascade",
                            sub.data.type = "suppress",
                            denominator = "aware",
                            suffix = "",
                            include.countries = T,
                            age  = c("0-14"),
                            sex="")

all.ages.test = read.cascade.data.files(dir = 'data_manager/data',
                            data.type="cascade",
                            sub.data.type = "suppress",
                            denominator = "aware",
                            suffix = "",
                            include.countries = T,
                            age  = c("All ages"),
                            sex="All")
all.ages.test = all.ages.test[,"South Africa"]


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
DATA.MANAGER$suppression$year.location[,"South Africa"] # this is suppression out of PEOPLE ON ART - INCORRECT!! 
DATA.MANAGER$engagement$year.location[,"South Africa"] # this is out of AWARENESS - CORRECT 

suppress.copy = DATA.MANAGER$suppression$year.location[,"South Africa"]
rv = DATA.MANAGER
new.suppress = (rv$suppression$year.location*rv$engagement$year.location)
new.suppress = new.suppress[,"South Africa"]

simplot(sim,   
    years=1980:2030, 
    data.types=c("suppression"), 
    proportion=T) +geom_hline(yintercept = .64226759031) +geom_hline(yintercept = 0.84) + geom_vline(xintercept = 2015)
# 2015 sim value and data value 
# fixed 2015 data value: 0.5695 (old value: 0.84)

sim.numerator/sim.denominator


