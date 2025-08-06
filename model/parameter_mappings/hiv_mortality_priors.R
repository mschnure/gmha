LOCATIONS = c("Kenya","South Africa","France","Mozambique","Tanzania",
              "Uganda","Zambia","Zimbabwe","Malawi","Nigeria","Thailand",
              "Cambodia",
              "unaids.remainder","non.unaids.remainder",
              "r1.low","r1.lower.middle","r1.upper.middle","r1.high",
              "Global")

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

sapply(HIV.MORTALITY.PRIORS,is.na)
ratio.time.2.to.1 = HIV.MORTALITY.PRIORS[["Global"]][2]/HIV.MORTALITY.PRIORS[["Global"]][1] # 2 is roughly 1.6 1
ratio.time.3.to.1 = HIV.MORTALITY.PRIORS[["Global"]][3]/HIV.MORTALITY.PRIORS[["Global"]][1] # 3 is roughly 0.44x 1

ratio.time.1.to.2 = HIV.MORTALITY.PRIORS[["Global"]][1]/HIV.MORTALITY.PRIORS[["Global"]][2] # 1 is roughly 1/2 2

# was going to use these ratios, but instead taking from when I calculated this from the suppressed data 
HIV.MORTALITY.PRIORS$France[2] = 0.002272727 # ratio.time.2.to.1*HIV.MORTALITY.PRIORS$France[1] # 0.002272727
HIV.MORTALITY.PRIORS$France[3] = 0.003947368 # ratio.time.3.to.1*HIV.MORTALITY.PRIORS$France[1] # 0.003947368

HIV.MORTALITY.PRIORS$Cambodia[1] = ratio.time.1.to.2*HIV.MORTALITY.PRIORS$Cambodia[2] # 0.03415152

HIV.MORTALITY.PRIORS$r1.high[3] = HIV.MORTALITY.PRIORS$r1.high[2]

rm(LOCATION)

    

