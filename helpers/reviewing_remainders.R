
# UNAIDS remainder
    # incidence: 8-11%, 2023: 21%
    # prevalence: 3-7%, 2023: 10% 
# NON-UNAIDS remainder
    # incidence: 4-28%
    # prevalence: 4-12%
# R1 LOW
    # incidence: 1-14%
    # prevalence: <1% 
# R1 LOWER MIDDLE
    # incidence: 3-8% 
    # prevalence: <1% 
# R1 UPPER MIDDLE
    # incidence: 25-35%
    # prevalence: 25-35%
# R1 HIGH
    # incidence: NA's
    # prevalence: NA's
# Kenya, as an example
    # incidence: 1-2%
    # prevalence: 2-3% 


# UNAIDS REMAINDER - total #
# incidence
x = DATA.MANAGER$incidence$year.age.location[,,"unaids.remainder"]
x = x[,c(1,4,6)]
y = apply(x,"year",sum)
z = DATA.MANAGER$incidence$year.location[,"unaids.remainder"]
z-y  # positive means totals are higher than age-specific sums 
((z-y)/z)*100 # percent that they are off of the total 
    # 8-11%, except 2023 is off by 21% 

# prevalence
x = DATA.MANAGER$prevalence$year.age.location[,,"unaids.remainder"]
x = x[,c(1,4,6)]
y = apply(x,"year",sum)
z = DATA.MANAGER$prevalence$year.location[,"unaids.remainder"]
z-y 
((z-y)/z)*100 # 3-7%; except 2023 off by 10% 

# NON-UNAIDS REMAINDER
# incidence
x = DATA.MANAGER$incidence$year.age.location[,,"non.unaids.remainder"]
x = x[,c(1,4,6)]
y = apply(x,"year",sum)
z = DATA.MANAGER$incidence$year.location[,"non.unaids.remainder"]
z-y 
((z-y)/z)*100 # 4-28% !

# prevalence
x = DATA.MANAGER$prevalence$year.age.location[,,"non.unaids.remainder"]
x = x[,c(1,4,6)]
y = apply(x,"year",sum)
z = DATA.MANAGER$prevalence$year.location[,"non.unaids.remainder"]
z-y 
((z-y)/z)*100 # 4-12% 

# R1 LOW
# incidence
x = DATA.MANAGER$incidence$year.age.location[,,"r1.low"]
x = x[,c(1,4,6)]
y = apply(x,"year",sum)
z = DATA.MANAGER$incidence$year.location[,"r1.low"]
z-y 
((z-y)/z)*100 # 1-14%

# prevalence
x = DATA.MANAGER$prevalence$year.age.location[,,"r1.low"]
x = x[,c(1,4,6)]
y = apply(x,"year",sum)
z = DATA.MANAGER$prevalence$year.location[,"r1.low"]
z-y 
((z-y)/z)*100 # <1% 


# R1 LOWER MIDDLE
# incidence
x = DATA.MANAGER$incidence$year.age.location[,,"r1.lower.middle"]
x = x[,c(1,4,6)]
y = apply(x,"year",sum)
z = DATA.MANAGER$incidence$year.location[,"r1.lower.middle"]
z-y 
((z-y)/z)*100 # 3-8% 

# prevalence
x = DATA.MANAGER$prevalence$year.age.location[,,"r1.lower.middle"]
x = x[,c(1,4,6)]
y = apply(x,"year",sum)
z = DATA.MANAGER$prevalence$year.location[,"r1.lower.middle"]
z-y 
((z-y)/z)*100 # <1% 

# R1 UPPER MIDDLE
# incidence
x = DATA.MANAGER$incidence$year.age.location[,,"r1.upper.middle"]
x = x[,c(1,4,6)]
y = apply(x,"year",sum)
z = DATA.MANAGER$incidence$year.location[,"r1.upper.middle"]
z-y 
((z-y)/z)*100 # 25-35%!

# prevalence
x = DATA.MANAGER$prevalence$year.age.location[,,"r1.upper.middle"]
x = x[,c(1,4,6)]
y = apply(x,"year",sum)
z = DATA.MANAGER$prevalence$year.location[,"r1.upper.middle"]
z-y 
((z-y)/z)*100 # 25-35% !

# R1 HIGH
# incidence
x = DATA.MANAGER$incidence$year.age.location[,,"r1.high"]
x = x[,c(1,4,6)]
y = apply(x,"year",sum)
z = DATA.MANAGER$incidence$year.location[,"r1.high"]
z-y 
((z-y)/z)*100 # too many NA's

# prevalence
x = DATA.MANAGER$prevalence$year.age.location[,,"r1.high"]
x = x[,c(1,4,6)]
y = apply(x,"year",sum)
z = DATA.MANAGER$prevalence$year.location[,"r1.high"]
z-y 
((z-y)/z)*100 # too many NA's


# Kenya, as an example
# incidence
x = DATA.MANAGER$incidence$year.age.location[,,"Kenya"]
x = x[,c(1,4,6)]
y = apply(x,"year",sum)
z = DATA.MANAGER$incidence$year.location[,"Kenya"]
z-y 
((z-y)/z)*100 # 1-2% 

# prevalence
x = DATA.MANAGER$prevalence$year.age.location[,,"Kenya"]
x = x[,c(1,4,6)]
y = apply(x,"year",sum)
z = DATA.MANAGER$prevalence$year.location[,"Kenya"]
z-y 
((z-y)/z)*100 # 2-3% (except 2021, which is 6%)
