source("model/run_systematic.R")

load("cached/data.manager_global_2026-06-25.Rdata")

DATA.MANAGER.OLD = DATA.MANAGER

new.ages = c("0-14","10-19","15-24","25-49","50 and over")
new.lowers = c(0,10,15,25,50)
new.uppers = c(15,20,25,50,Inf)

DATA.MANAGER$incidence$AGES = DATA.MANAGER$prevalence$AGES = DATA.MANAGER$hiv.mortality$AGES = 
  DATA.MANAGER$incidence.lowers$AGES = DATA.MANAGER$incidence.uppers$AGES = 
  DATA.MANAGER$prevalence.lowers$AGES = DATA.MANAGER$prevalence.uppers$AGES = 
  DATA.MANAGER$hiv.mortality.lowers$AGES = DATA.MANAGER$hiv.mortality.uppers$AGES = new.ages

DATA.MANAGER$incidence$AGE.LOWERS = DATA.MANAGER$prevalence$AGE.LOWERS = DATA.MANAGER$hiv.mortality$AGE.LOWERS = 
  DATA.MANAGER$incidence.lowers$AGE.LOWERS = DATA.MANAGER$incidence.uppers$AGE.LOWERS = 
  DATA.MANAGER$prevalence.lowers$AGE.LOWERS = DATA.MANAGER$prevalence.uppers$AGE.LOWERS = 
  DATA.MANAGER$hiv.mortality.lowers$AGE.LOWERS = DATA.MANAGER$hiv.mortality.uppers$AGE.LOWERS = new.lowers

DATA.MANAGER$incidence$AGE.UPPERS = DATA.MANAGER$prevalence$AGE.UPPERS = DATA.MANAGER$hiv.mortality$AGE.UPPERS = 
  DATA.MANAGER$incidence.lowers$AGE.UPPERS = DATA.MANAGER$incidence.uppers$AGE.UPPERS = 
  DATA.MANAGER$prevalence.lowers$AGE.UPPERS = DATA.MANAGER$prevalence.uppers$AGE.UPPERS = 
  DATA.MANAGER$hiv.mortality.lowers$AGE.UPPERS = DATA.MANAGER$hiv.mortality.uppers$AGE.UPPERS = new.uppers

# INCIDENCE (estimates, then lowers and uppers): year.age.location
{
  # point estimate
  incidence.age = DATA.MANAGER$incidence$year.age.location
  new.25.to.49.incidence = incidence.age[,"15-49",] - incidence.age[,"15-24",]
  
  new.incidence.age =  array(NA,
                             dim = c(dim(incidence.age)[1], length(new.ages), dim(incidence.age)[3]),
                             dimnames = list(dimnames(incidence.age)[[1]],  # year
                                             new.ages,                       # new age categories
                                             dimnames(incidence.age)[[3]]    # location
                             )
  )
  
  new.incidence.age[, "0-14", ] = incidence.age[, "0-14", ]
  new.incidence.age[, "10-19", ] = incidence.age[, "10-19", ]
  new.incidence.age[, "15-24", ] = incidence.age[, "15-24", ]
  new.incidence.age[, "25-49", ] = new.25.to.49.incidence
  new.incidence.age[, "50 and over", ] = incidence.age[, "50 and over", ]
  
  # lowers
  incidence.lowers.age = DATA.MANAGER$incidence.lowers$year.age.location
  new.25.to.49.incidence.lowers = incidence.lowers.age[,"15-49",] - incidence.lowers.age[,"15-24",]
  
  new.incidence.lowers.age =  array(NA,
                                    dim = c(dim(incidence.lowers.age)[1], length(new.ages), dim(incidence.lowers.age)[3]),
                                    dimnames = list(dimnames(incidence.lowers.age)[[1]],  # year
                                                    new.ages,                       # new age categories
                                                    dimnames(incidence.lowers.age)[[3]]    # location
                                    )
  )
  
  new.incidence.lowers.age[, "0-14", ] = incidence.lowers.age[, "0-14", ]
  new.incidence.lowers.age[, "10-19", ] = incidence.lowers.age[, "10-19", ]
  new.incidence.lowers.age[, "15-24", ] = incidence.lowers.age[, "15-24", ]
  new.incidence.lowers.age[, "25-49", ] = new.25.to.49.incidence.lowers
  new.incidence.lowers.age[, "50 and over", ] = incidence.lowers.age[, "50 and over", ]
  
  # uppers
  incidence.uppers.age = DATA.MANAGER$incidence.uppers$year.age.location
  new.25.to.49.incidence.uppers = incidence.uppers.age[,"15-49",] - incidence.uppers.age[,"15-24",]
  
  new.incidence.uppers.age =  array(NA,
                                    dim = c(dim(incidence.uppers.age)[1], length(new.ages), dim(incidence.uppers.age)[3]),
                                    dimnames = list(dimnames(incidence.uppers.age)[[1]],  # year
                                                    new.ages,                       # new age categories
                                                    dimnames(incidence.uppers.age)[[3]]    # location
                                    )
  )
  
  new.incidence.uppers.age[, "0-14", ] = incidence.uppers.age[, "0-14", ]
  new.incidence.uppers.age[, "10-19", ] = incidence.uppers.age[, "10-19", ]
  new.incidence.uppers.age[, "15-24", ] = incidence.uppers.age[, "15-24", ]
  new.incidence.uppers.age[, "25-49", ] = new.25.to.49.incidence.uppers
  new.incidence.uppers.age[, "50 and over", ] = incidence.uppers.age[, "50 and over", ]
  
}


# PREVALENCE (estimates, then lowers and uppers): year.age.location
{
  # point estimate
  prevalence.age = DATA.MANAGER$prevalence$year.age.location
  new.25.to.49.prevalence = prevalence.age[,"15-49",] - prevalence.age[,"15-24",]
  
  new.prevalence.age =  array(NA,
                              dim = c(dim(prevalence.age)[1], length(new.ages), dim(prevalence.age)[3]),
                              dimnames = list(dimnames(prevalence.age)[[1]],  # year
                                              new.ages,                       # new age categories
                                              dimnames(prevalence.age)[[3]]    # location
                              )
  )
  
  new.prevalence.age[, "0-14", ] = prevalence.age[, "0-14", ]
  new.prevalence.age[, "10-19", ] = prevalence.age[, "10-19", ]
  new.prevalence.age[, "15-24", ] = prevalence.age[, "15-24", ]
  new.prevalence.age[, "25-49", ] = new.25.to.49.prevalence
  new.prevalence.age[, "50 and over", ] = prevalence.age[, "50 and over", ]
  
  # lowers
  prevalence.lowers.age = DATA.MANAGER$prevalence.lowers$year.age.location
  new.25.to.49.prevalence.lowers = prevalence.lowers.age[,"15-49",] - prevalence.lowers.age[,"15-24",]
  
  new.prevalence.lowers.age =  array(NA,
                                     dim = c(dim(prevalence.lowers.age)[1], length(new.ages), dim(prevalence.lowers.age)[3]),
                                     dimnames = list(dimnames(prevalence.lowers.age)[[1]],  # year
                                                     new.ages,                       # new age categories
                                                     dimnames(prevalence.lowers.age)[[3]]    # location
                                     )
  )
  
  new.prevalence.lowers.age[, "0-14", ] = prevalence.lowers.age[, "0-14", ]
  new.prevalence.lowers.age[, "10-19", ] = prevalence.lowers.age[, "10-19", ]
  new.prevalence.lowers.age[, "15-24", ] = prevalence.lowers.age[, "15-24", ]
  new.prevalence.lowers.age[, "25-49", ] = new.25.to.49.prevalence.lowers
  new.prevalence.lowers.age[, "50 and over", ] = prevalence.lowers.age[, "50 and over", ]
  
  # uppers
  prevalence.uppers.age = DATA.MANAGER$prevalence.uppers$year.age.location
  new.25.to.49.prevalence.uppers = prevalence.uppers.age[,"15-49",] - prevalence.uppers.age[,"15-24",]
  
  new.prevalence.uppers.age =  array(NA,
                                     dim = c(dim(prevalence.uppers.age)[1], length(new.ages), dim(prevalence.uppers.age)[3]),
                                     dimnames = list(dimnames(prevalence.uppers.age)[[1]],  # year
                                                     new.ages,                       # new age categories
                                                     dimnames(prevalence.uppers.age)[[3]]    # location
                                     )
  )
  
  new.prevalence.uppers.age[, "0-14", ] = prevalence.uppers.age[, "0-14", ]
  new.prevalence.uppers.age[, "10-19", ] = prevalence.uppers.age[, "10-19", ]
  new.prevalence.uppers.age[, "15-24", ] = prevalence.uppers.age[, "15-24", ]
  new.prevalence.uppers.age[, "25-49", ] = new.25.to.49.prevalence.uppers
  new.prevalence.uppers.age[, "50 and over", ] = prevalence.uppers.age[, "50 and over", ]
  
  
}


# HIV.MORTALITY (estimates, then lowers and uppers): year.age.location
{
  # point estimate
  hiv.mortality.age = DATA.MANAGER$hiv.mortality$year.age.location
  new.25.to.49.hiv.mortality = hiv.mortality.age[,"15-49",] - hiv.mortality.age[,"15-24",]
  
  new.hiv.mortality.age =  array(NA,
                                 dim = c(dim(hiv.mortality.age)[1], length(new.ages), dim(hiv.mortality.age)[3]),
                                 dimnames = list(dimnames(hiv.mortality.age)[[1]],  # year
                                                 new.ages,                       # new age categories
                                                 dimnames(hiv.mortality.age)[[3]]    # location
                                 )
  )
  
  new.hiv.mortality.age[, "0-14", ] = hiv.mortality.age[, "0-14", ]
  new.hiv.mortality.age[, "10-19", ] = hiv.mortality.age[, "10-19", ]
  new.hiv.mortality.age[, "15-24", ] = hiv.mortality.age[, "15-24", ]
  new.hiv.mortality.age[, "25-49", ] = new.25.to.49.hiv.mortality
  new.hiv.mortality.age[, "50 and over", ] = hiv.mortality.age[, "50 and over", ]
  
  # lowers
  hiv.mortality.lowers.age = DATA.MANAGER$hiv.mortality.lowers$year.age.location
  new.25.to.49.hiv.mortality.lowers = hiv.mortality.lowers.age[,"15-49",] - hiv.mortality.lowers.age[,"15-24",]
  
  new.hiv.mortality.lowers.age =  array(NA,
                                        dim = c(dim(hiv.mortality.lowers.age)[1], length(new.ages), dim(hiv.mortality.lowers.age)[3]),
                                        dimnames = list(dimnames(hiv.mortality.lowers.age)[[1]],  # year
                                                        new.ages,                       # new age categories
                                                        dimnames(hiv.mortality.lowers.age)[[3]]    # location
                                        )
  )
  
  new.hiv.mortality.lowers.age[, "0-14", ] = hiv.mortality.lowers.age[, "0-14", ]
  new.hiv.mortality.lowers.age[, "10-19", ] = hiv.mortality.lowers.age[, "10-19", ]
  new.hiv.mortality.lowers.age[, "15-24", ] = hiv.mortality.lowers.age[, "15-24", ]
  new.hiv.mortality.lowers.age[, "25-49", ] = new.25.to.49.hiv.mortality.lowers
  new.hiv.mortality.lowers.age[, "50 and over", ] = hiv.mortality.lowers.age[, "50 and over", ]
  
  # uppers
  hiv.mortality.uppers.age = DATA.MANAGER$hiv.mortality.uppers$year.age.location
  new.25.to.49.hiv.mortality.uppers = hiv.mortality.uppers.age[,"15-49",] - hiv.mortality.uppers.age[,"15-24",]
  
  new.hiv.mortality.uppers.age =  array(NA,
                                        dim = c(dim(hiv.mortality.uppers.age)[1], length(new.ages), dim(hiv.mortality.uppers.age)[3]),
                                        dimnames = list(dimnames(hiv.mortality.uppers.age)[[1]],  # year
                                                        new.ages,                       # new age categories
                                                        dimnames(hiv.mortality.uppers.age)[[3]]    # location
                                        )
  )
  
  new.hiv.mortality.uppers.age[, "0-14", ] = hiv.mortality.uppers.age[, "0-14", ]
  new.hiv.mortality.uppers.age[, "10-19", ] = hiv.mortality.uppers.age[, "10-19", ]
  new.hiv.mortality.uppers.age[, "15-24", ] = hiv.mortality.uppers.age[, "15-24", ]
  new.hiv.mortality.uppers.age[, "25-49", ] = new.25.to.49.hiv.mortality.uppers
  new.hiv.mortality.uppers.age[, "50 and over", ] = hiv.mortality.uppers.age[, "50 and over", ]
  
}

DATA.MANAGER$incidence$year.age.location = new.incidence.age
DATA.MANAGER$incidence.lowers$year.age.location = new.incidence.lowers.age
DATA.MANAGER$incidence.uppers$year.age.location = new.incidence.uppers.age

DATA.MANAGER$prevalence$year.age.location = new.prevalence.age
DATA.MANAGER$prevalence.lowers$year.age.location = new.prevalence.lowers.age
DATA.MANAGER$prevalence.uppers$year.age.location = new.prevalence.uppers.age

DATA.MANAGER$hiv.mortality$year.age.location = new.hiv.mortality.age
DATA.MANAGER$hiv.mortality.lowers$year.age.location = new.hiv.mortality.lowers.age
DATA.MANAGER$hiv.mortality.uppers$year.age.location = new.hiv.mortality.uppers.age


save(DATA.MANAGER,file=paste0("cached/data.manager_global_revised_ages_",Sys.Date(),".Rdata"))
