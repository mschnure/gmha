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

names(DATA.MANAGER$suppression)# need to ADD year.age.location and year.age to include 0-14 
DATA.MANAGER$suppression$AGES # need to update to include 0-14 (and 15-24?)
DATA.MANAGER$suppression$AGE.LOWERS 
DATA.MANAGER$suppression$AGE.UPPERS

# this is % of PEOPLE ON ART who achieve viral suppression
# in the data manager functions, I multiply this by engagement to get the right denominator: 
    # multiply (suppress/on ART) * (on ART/aware) in order to make denominator aware: 
children.supp.denom.eng = read.cascade.data.files(dir = 'data_manager/data',
                                                  data.type="cascade",
                                                  sub.data.type = "suppress",
                                                  denominator = "aware",
                                                  suffix = "",
                                                  include.countries = T,
                                                  age  = c("0-14"),
                                                  sex="")
#children.supp.denom.eng[,"South Africa"]
children.eng = read.cascade.data.files(dir = 'data_manager/data',
                                                  data.type="cascade",
                                                  sub.data.type = "ART",
                                                  denominator = "aware",
                                                  suffix = "",
                                                  include.countries = T,
                                                  age  = c("0-14"),
                                                  sex="")
children.supp.denom.aware = children.supp.denom.eng*children.eng
children.supp.denom.aware = children.supp.denom.aware[,"South Africa"]
children.supp.denom.aware
DATA.MANAGER$suppression$year.age.location[,,"South Africa"] 

dim(DATA.MANAGER$suppression$year.age)
dim(DATA.MANAGER$suppression$year.age.location)

# check how I do it for all ages: 
all.ages.supp.denom.eng = read.cascade.data.files(dir = 'data_manager/data',
                                                  data.type="cascade",
                                                  sub.data.type = "suppress",
                                                  denominator = "aware",
                                                  suffix = "",
                                                  include.countries = T,
                                                  age  = c("All ages"),
                                                  sex="All")
all.ages.eng = read.cascade.data.files(dir = 'data_manager/data',
                                        data.type="cascade",
                                        sub.data.type = "ART",
                                        denominator = "aware",
                                        suffix = "",
                                        include.countries = T,
                                        age  = c("All ages"),
                                        sex="All")
all.ages.supp.denom.aware = all.ages.supp.denom.eng*all.ages.eng
all.ages.supp.denom.aware = all.ages.supp.denom.aware[,"South Africa"]

# these should be the same - good 
all.ages.supp.denom.aware
DATA.MANAGER$suppression$year.location[,"South Africa"]



