

INDIVIDUAL.COUNTRIES = c("South Africa","Mozambique","Nigeria","Tanzania","Uganda","Kenya",
                            "Zambia","Zimbabwe","Malawi")

# need an old version of the DATA.MANAGER loaded to do this 
REMAINDER.COUNTRIES.UNAIDS = DATA.MANAGER$prevalence$LOCATIONS[!(DATA.MANAGER$prevalence$LOCATIONS %in% INDIVIDUAL.COUNTRIES)]
REMAINDER.COUNTRIES.UNAIDS = REMAINDER.COUNTRIES.UNAIDS[!grepl("remainder",REMAINDER.COUNTRIES.UNAIDS)]

save(REMAINDER.COUNTRIES.UNAIDS,file="data_manager/remainder_countries.Rdata")


