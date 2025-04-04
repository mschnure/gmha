

INDIVIDUAL.COUNTRIES = c("South Africa","Mozambique","Nigeria","Tanzania","Uganda","Kenya",
                            "Zambia","Zimbabwe","Malawi")

# need an old version of the DATA.MANAGER loaded to do this 
REMAINDER.COUNTRIES.UNAIDS = DATA.MANAGER$prevalence$LOCATIONS[!(DATA.MANAGER$prevalence$LOCATIONS %in% INDIVIDUAL.COUNTRIES)]
REMAINDER.COUNTRIES.UNAIDS = REMAINDER.COUNTRIES.UNAIDS[!grepl("remainder",REMAINDER.COUNTRIES.UNAIDS)]

save(REMAINDER.COUNTRIES.UNAIDS,file="data_manager/remainder_countries.Rdata")


indiv.prev = apply(DATA.MANAGER$prevalence$year.location[,INDIVIDUAL.COUNTRIES],1,sum)
unaids.prev = apply(DATA.MANAGER$prevalence$year.location[,REMAINDER.COUNTRIES.UNAIDS],1,sum,na.rm=T)
global.prev = DATA.MANAGER$prevalence$year
non.unaids.prev = global.prev - (indiv.prev + unaids.prev)

# values for weighting cascade percentages
indiv.weight = indiv.prev/global.prev
unaids.remainder.weight = unaids.prev/global.prev
non.unaids.remainder.weight = non.unaids.prev/global.prev

weight.dim.names = list("year" = names(indiv.weight),
                        "weight" = c("indiv","unaids","non.unaids"))

CASCADE.WEIGHTS = array(c(indiv.weight,unaids.remainder.weight,non.unaids.remainder.weight),
                        dim = sapply(weight.dim.names,length),
                        dimnames = weight.dim.names)
save(CASCADE.WEIGHTS,file="data_manager/cascade_weights.Rdata")
