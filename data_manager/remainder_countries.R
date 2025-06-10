load("data_manager/locations_income.Rdata")
load("data_manager/remainder_countries.Rdata") # if doing for the first time, run lines 1==2 below 

INDIVIDUAL.COUNTRIES = c("South Africa","Mozambique","Nigeria","Tanzania","Uganda","Kenya",
                            "Zambia","Zimbabwe","Malawi")

# need an old version of the DATA.MANAGER loaded to do this 
if(1==2){
    REMAINDER.COUNTRIES.UNAIDS = DATA.MANAGER$prevalence$LOCATIONS[!(DATA.MANAGER$prevalence$LOCATIONS %in% INDIVIDUAL.COUNTRIES)]
    REMAINDER.COUNTRIES.UNAIDS = REMAINDER.COUNTRIES.UNAIDS[!grepl("remainder",REMAINDER.COUNTRIES.UNAIDS)]
    
    save(REMAINDER.COUNTRIES.UNAIDS,file="data_manager/remainder_countries.Rdata")    
}

indiv.prev = apply(DATA.MANAGER$prevalence$year.location[,INDIVIDUAL.COUNTRIES],1,sum)
unaids.prev = apply(DATA.MANAGER$prevalence$year.location[,REMAINDER.COUNTRIES.UNAIDS],1,sum,na.rm=T)
global.prev = DATA.MANAGER$prevalence$year
non.unaids.prev = global.prev - (indiv.prev + unaids.prev)

low.remainder.prev = apply(DATA.MANAGER$prevalence$year.location[,LOCATIONS.INCOME$low.remainder],1,sum,na.rm=T)
lower.middle.remainder.prev = apply(DATA.MANAGER$prevalence$year.location[,LOCATIONS.INCOME$lower.middle.remainder],1,sum,na.rm=T)
upper.middle.remainder.prev = apply(DATA.MANAGER$prevalence$year.location[,LOCATIONS.INCOME$upper.middle.remainder],1,sum,na.rm=T)
high.remainder.prev = apply(DATA.MANAGER$prevalence$year.location[,LOCATIONS.INCOME$high.remainder],1,sum,na.rm=T)

# values for weighting cascade percentages
indiv.weight = indiv.prev/global.prev
unaids.remainder.weight = unaids.prev/global.prev
non.unaids.remainder.weight = non.unaids.prev/global.prev

low.remainder.weight = low.remainder.prev/global.prev
lower.middle.remainder.weight = lower.middle.remainder.prev/global.prev
upper.middle.remainder.weight = upper.middle.remainder.prev/global.prev
high.remainder.weight = high.remainder.prev/global.prev

weight.dim.names = list("year" = names(indiv.weight),
                        "weight" = c("indiv","unaids","non.unaids",
                                     "r1.low","r1.lower.middle","r1.upper.middle","r1.high"))

CASCADE.WEIGHTS = array(c(indiv.weight,unaids.remainder.weight,non.unaids.remainder.weight,
                          low.remainder.weight,lower.middle.remainder.weight,
                          upper.middle.remainder.weight,high.remainder.weight),
                        dim = sapply(weight.dim.names,length),
                        dimnames = weight.dim.names)
save(CASCADE.WEIGHTS,file="data_manager/cascade_weights.Rdata")
