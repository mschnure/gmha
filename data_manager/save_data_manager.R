source('data_manager/extract_suppressed_values.R')
source('helpers/convert_strings.R')
source('data_manager/data_manager_functions.R')
source('model/age_mappings.R') 
source('data_manager/scaling_remainder_models.R')
load('data_manager/remainder_countries.Rdata')
load('data_manager/cascade_weights.Rdata')
load('data_manager/locations_income.Rdata')

## Countries with no UNAIDS PLHIV estimates (China, Russia, United States, Japan, UK, Turkiye, Cameroon, ...) are
## modeled in non.unaids.remainder: their PLHIV already land there (global - reported), so take them out of the
## remainder/income lists so their population and deaths go there too (otherwise r1.* population includes them
## while r1.* PLHIV does not). Added 10/15/26.
NO.PLHIV.DATA.COUNTRIES = get.countries.without.prevalence.data()
REMAINDER.COUNTRIES.UNAIDS = setdiff(REMAINDER.COUNTRIES.UNAIDS, NO.PLHIV.DATA.COUNTRIES)
for(group in setdiff(names(LOCATIONS.INCOME), "full"))
    LOCATIONS.INCOME[[group]] = setdiff(LOCATIONS.INCOME[[group]], NO.PLHIV.DATA.COUNTRIES)
print(paste0("Moved to non.unaids.remainder (no PLHIV data): ", paste0(NO.PLHIV.DATA.COUNTRIES, collapse = ", ")))

EXTRACT.SUPPRESSED.VALUES = T 
SCALE.REMAINDER.DATA = T

## all countries other than these individual countries will be modeled as the remainder
## if you want to change this, need to edit data_manager/remainder_countries.R" file 
INDIVIDUAL.COUNTRIES = c("South Africa","Mozambique","Nigeria","Tanzania","Uganda","Kenya",
                         "Zambia","Zimbabwe","Malawi") # removed France, Thailand, Cambodia, Brazil, Chile, Netherlands
                                                                
COUNTRIES.TO.PULL.PDFS = c("South Africa","Mozambique","Nigeria","Tanzania","Uganda","Kenya",
                           "Zambia","Zimbabwe","Malawi", # "France","Thailand", "Cambodia",
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


# non.unaids.remainder bounds relative to Global; clamp any lower > point / upper < point (10/15/26)
DATA.MANAGER = fix.count.bounds(DATA.MANAGER)

## NEXTGEN: flag this as the LAI version of the data manager (0-14 cascade data + LAI/oral suppression placeholders)
## - stored on the object as attr(DATA.MANAGER, "lai") so source_code.R can check it
## - and in the file name: cached/data.manager_global_lai_<date>.Rdata
LAI.DATA.MANAGER = T
attr(DATA.MANAGER, "lai") = LAI.DATA.MANAGER
DM.FILE.TAG = ifelse(LAI.DATA.MANAGER, "lai_", "")

save(DATA.MANAGER,file=paste0("cached/data.manager_global_",DM.FILE.TAG,Sys.Date(),".Rdata"))



# countries with diff names; use what's in UNAIDS data 
# population data --> change to --> what's in UNAIDS data 
    # United Republic of Tanzania --> change to -> Tanzania 
    # Dem. People's Republic of Korea --> change to --> Democratic People's Republic of Korea
    # Turkey --> change to --> Türkiye
    # United States of America --> change to --> United States
