source('data_manager/extract_suppressed_values.R')
source('helpers/convert_strings.R')
source('data_manager/data_manager_functions.R')
source('model/age_mappings.R') 
load('data_manager/remainder_countries.Rdata')
load('data_manager/cascade_weights.Rdata')

## all countries other than these individual countries will be modeled as the remainder
## if you want to change this, need to edit data_manager/remainder_countries.R" file 
INDIVIDUAL.COUNTRIES = c("South Africa","Mozambique","Nigeria","Tanzania","Uganda","Kenya",
                         "Zambia","Zimbabwe","Malawi") # removed Brazil, France, Cambodia, Chile, Thailand, Netherlands

COUNTRIES.TO.PULL.PDFS = c("South Africa","Mozambique","Nigeria","Tanzania","Uganda","Kenya",
                           "Zambia","Zimbabwe","Malawi","France","UNAIDS remainder","Non UNAIDS remainder")
# remainder countries both have NAs for PDFs for now 

DATA.MANAGER = read.surveillance.data() 

save(DATA.MANAGER,file=paste0("cached/data.manager_global_",Sys.Date(),".Rdata"))

