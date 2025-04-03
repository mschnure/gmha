x = rowSums(DATA.MANAGER$population$year.location[,INDIVIDUAL.COUNTRIES])
y = DATA.MANAGER$population$year.location[,"unaids.remainder"]
z = DATA.MANAGER$population$year.location[,"non.unaids.remainder"]
a = DATA.MANAGER$population$year
a - (x+y+z)
xx = a - (x+y+z)
xx/a


x = rowSums(DATA.MANAGER$total.mortality$year.location[,INDIVIDUAL.COUNTRIES])
y = DATA.MANAGER$total.mortality$year.location[,"unaids.remainder"]
z = DATA.MANAGER$total.mortality$year.location[,"non.unaids.remainder"]
a = DATA.MANAGER$total.mortality$year

x = apply(DATA.MANAGER$total.mortality$year.age.sex.location[,,,INDIVIDUAL.COUNTRIES],c(1),sum)
y = apply(DATA.MANAGER$total.mortality$year.age.sex.location[,,,"unaids.remainder"],c(1),sum)
z = apply(DATA.MANAGER$total.mortality$year.age.sex.location[,,,"non.unaids.remainder"],c(1),sum)
a = apply(DATA.MANAGER$total.mortality$year.age.sex,c(1),sum)
a - (x+y+z)
xx = a - (x+y+z)
xx/a
