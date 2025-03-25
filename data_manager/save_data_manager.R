source('data_manager/extract_suppressed_values.R')
source('helpers/convert_strings.R')
source('data_manager/data_manager_functions.R')
source('model/age_mappings.R') 
source('calibration/scaling_prevalence.R')

scale.data = F # CHECK BACK ON THIS LATER

COUNTRIES.TO.PULL.POP = c("Cambodia","Kenya","South Africa","Chile","Thailand","Netherlands","France",
                          "Mozambique","Tanzania","Uganda")
COUNTRIES.TO.PULL.PDFS = c("Kenya","South Africa","France",
                           "Mozambique","Tanzania","Uganda")

DATA.MANAGER = read.surveillance.data() 

if(scale.data){
    DATA.MANAGER$prevalence = scale.calibration.data(data.type = "prevalence",location = "Kenya")
    DATA.MANAGER$prevalence.lowers = scale.calibration.data(data.type = "prevalence.lowers",location = "Kenya")
    DATA.MANAGER$prevalence.uppers = scale.calibration.data(data.type = "prevalence.uppers",location = "Kenya")
    
    DATA.MANAGER$incidence = scale.calibration.data(data.type = "incidence",location = "Kenya")
    DATA.MANAGER$incidence.lowers = scale.calibration.data(data.type = "incidence.lowers",location = "Kenya")
    DATA.MANAGER$incidence.uppers = scale.calibration.data(data.type = "incidence.uppers",location = "Kenya")
    
    DATA.MANAGER$hiv.mortality = scale.calibration.data(data.type = "hiv.mortality",location = "Kenya")
    DATA.MANAGER$hiv.mortality.lowers = scale.calibration.data(data.type = "hiv.mortality.lowers",location = "Kenya")
    DATA.MANAGER$hiv.mortality.uppers = scale.calibration.data(data.type = "hiv.mortality.uppers",location = "Kenya")  
}

save(DATA.MANAGER,file=paste0("cached/data.manager_global_",Sys.Date(),".Rdata"))

