# browser into end of read.surveillance.data.files with     if(data.type=="prevalence") browser()

unaids.remainder.countries = rowSums(locations[,REMAINDER.COUNTRIES.UNAIDS],na.rm = T)

individual.countries = rowSums(locations[,INDIVIDUAL.COUNTRIES],na.rm = T)

unaids.percent = (unaids.remainder.countries + individual.countries)/global

non.unaids.remainder = global - rowSums(locations, na.rm = T)
non.unaids.remainder.percent = non.unaids.remainder/global

unaids.percent + non.unaids.remainder.percent

global.added = unaids.remainder.countries + individual.countries + non.unaids.remainder
global.added/global
