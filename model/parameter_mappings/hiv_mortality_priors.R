LOCATIONS = c("Kenya","South Africa","France","Mozambique","Tanzania",
              "Uganda","Zambia","Zimbabwe","Malawi","Nigeria","unaids.remainder","non.unaids.remainder")

HIV.MORTALITY.PRIORS = list()

for(LOCATION in LOCATIONS){

    # 1990 AIDS-related deaths/1990 prevalence
    hiv.mortality.1990 = DATA.MANAGER$hiv.mortality$year.location["1990",LOCATION]/
        DATA.MANAGER$prevalence$year.location["1990",LOCATION]
    
    # 2005 AIDS-related deaths/1990 prevalence
    hiv.mortality.2005 = DATA.MANAGER$hiv.mortality$year.location["2005",LOCATION]/
        DATA.MANAGER$prevalence$year.location["2005",LOCATION]
    
    # 2020 AIDS-related deaths/1990 prevalence
    hiv.mortality.2020 = DATA.MANAGER$hiv.mortality$year.location["2020",LOCATION]/
        DATA.MANAGER$prevalence$year.location["2020",LOCATION]
    
    HIV.MORTALITY.PRIORS[[LOCATION]] = c("1990" = hiv.mortality.1990,
                                         "2005" = hiv.mortality.2005,
                                         "2020" = hiv.mortality.2020)
        
}

HIV.MORTALITY.PRIORS[["Nigeria"]]
