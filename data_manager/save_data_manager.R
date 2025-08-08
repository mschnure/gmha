source('data_manager/extract_suppressed_values.R')
source('helpers/convert_strings.R')
source('data_manager/data_manager_functions.R')
source('model/age_mappings.R') 
source('data_manager/scaling_remainder_models.R')
load('data_manager/remainder_countries.Rdata')
load('data_manager/cascade_weights.Rdata')
load('data_manager/locations_income.Rdata')

EXTRACT.SUPPRESSED.VALUES = F 
SCALE.REMAINDER.DATA = T

## all countries other than these individual countries will be modeled as the remainder
## if you want to change this, need to edit data_manager/remainder_countries.R" file 
INDIVIDUAL.COUNTRIES = c("South Africa","Mozambique","Nigeria","Tanzania","Uganda","Kenya",
                         "Zambia","Zimbabwe","Malawi","France","Thailand","Cambodia") # removed Brazil, Chile, Netherlands
                                                                
COUNTRIES.TO.PULL.PDFS = c("South Africa","Mozambique","Nigeria","Tanzania","Uganda","Kenya",
                           "Zambia","Zimbabwe","Malawi","France","Thailand", "Cambodia",
                           "UNAIDS remainder","Non UNAIDS remainder")
# remainder countries both have NAs for PDFs for now 

DATA.MANAGER = read.surveillance.data() 

if(SCALE.REMAINDER.DATA){
    locations.to.scale = c("unaids.remainder","non.unaids.remainder","r1.low","r1.lower.middle","r1.upper.middle","r1.high")
    for(location in locations.to.scale){
        DATA.MANAGER$prevalence$year.age.location[,,location] = scale.calibration.data(data.type = "prevalence",location = location)
        DATA.MANAGER$prevalence.lowers$year.age.location[,,location] = scale.calibration.data(data.type = "prevalence.lowers",location = location)
        DATA.MANAGER$prevalence.uppers$year.age.location[,,location] = scale.calibration.data(data.type = "prevalence.uppers",location = location)

        DATA.MANAGER$incidence$year.age.location[,,location] = scale.calibration.data(data.type = "incidence",location = location)
        DATA.MANAGER$incidence.lowers$year.age.location[,,location] = scale.calibration.data(data.type = "incidence.lowers",location = location)
        DATA.MANAGER$incidence.uppers$year.age.location[,,location] = scale.calibration.data(data.type = "incidence.uppers",location = location)
    }
}


save(DATA.MANAGER,file=paste0("cached/data.manager_global_",Sys.Date(),".Rdata"))



# countries with diff names; use what's in UNAIDS data 
# population data --> change to --> what's in UNAIDS data 
    # United Republic of Tanzania --> change to -> Tanzania 
    # Dem. People's Republic of Korea --> change to --> Democratic People's Republic of Korea
    # Turkey --> change to --> Türkiye
    # United States of America --> change to --> United States
