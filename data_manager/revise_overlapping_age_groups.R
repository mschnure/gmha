source("model/run_systematic.R")

# load the most recent (non-revised) data manager made by save_data_manager.R
base.dm.files = list.files("cached", pattern = "^data\\.manager_global_[0-9-]+\\.Rdata$")
print(paste0("Revising age groups in: ", max(base.dm.files)))
load(file.path("cached", max(base.dm.files)))
#load("cached/data.manager_global_2026-06-25.Rdata")

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
                             dim = c(dim(incidence.age)[1], "age"=length(new.ages), dim(incidence.age)[3]),
                             dimnames = list("year"=dimnames(incidence.age)[[1]],  # year
                                             "age"=new.ages,                       # new age categories
                                             "location"=dimnames(incidence.age)[[3]]    # location
                             )
  )
  
  new.incidence.age[, "0-14", ] = incidence.age[, "0-14", ]
  new.incidence.age[, "10-19", ] = incidence.age[, "10-19", ]
  new.incidence.age[, "15-24", ] = incidence.age[, "15-24", ]
  new.incidence.age[, "25-49", ] = new.25.to.49.incidence
  new.incidence.age[, "50 and over", ] = incidence.age[, "50 and over", ]
  
  # lowers
  incidence.lowers.age = DATA.MANAGER$incidence.lowers$year.age.location
  # bounds can't be subtracted (lower(15-49) - lower(15-24) is not a lower bound and often lands above the point);
  # instead give 25-49 the same relative interval as 15-49 (fixed 10/15/26)
  new.25.to.49.incidence.lowers = new.25.to.49.incidence * incidence.lowers.age[,"15-49",]/incidence.age[,"15-49",]
  
  new.incidence.lowers.age =  array(NA,
                                    dim = c(dim(incidence.lowers.age)[1], "age"=length(new.ages), dim(incidence.lowers.age)[3]),
                                    dimnames = list("year"=dimnames(incidence.lowers.age)[[1]],  # year
                                                    "age"=new.ages,                       # new age categories
                                                    "location"=dimnames(incidence.lowers.age)[[3]]    # location
                                    )
  )
  
  new.incidence.lowers.age[, "0-14", ] = incidence.lowers.age[, "0-14", ]
  new.incidence.lowers.age[, "10-19", ] = incidence.lowers.age[, "10-19", ]
  new.incidence.lowers.age[, "15-24", ] = incidence.lowers.age[, "15-24", ]
  new.incidence.lowers.age[, "25-49", ] = new.25.to.49.incidence.lowers
  new.incidence.lowers.age[, "50 and over", ] = incidence.lowers.age[, "50 and over", ]
  
  # uppers
  incidence.uppers.age = DATA.MANAGER$incidence.uppers$year.age.location
  # bounds can't be subtracted (lower(15-49) - lower(15-24) is not a lower bound and often lands above the point);
  # instead give 25-49 the same relative interval as 15-49 (fixed 10/15/26)
  new.25.to.49.incidence.uppers = new.25.to.49.incidence * incidence.uppers.age[,"15-49",]/incidence.age[,"15-49",]
  
  new.incidence.uppers.age =  array(NA,
                                    dim = c(dim(incidence.uppers.age)[1], "age"=length(new.ages), dim(incidence.uppers.age)[3]),
                                    dimnames = list("year"=dimnames(incidence.uppers.age)[[1]],  # year
                                                    "age"=new.ages,                       # new age categories
                                                    "location"=dimnames(incidence.uppers.age)[[3]]    # location
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
                              dim = c(dim(prevalence.age)[1], "age"=length(new.ages), dim(prevalence.age)[3]),
                              dimnames = list("year"=dimnames(prevalence.age)[[1]],  # year
                                              "age"=new.ages,                       # new age categories
                                              "location"=dimnames(prevalence.age)[[3]]    # location
                              )
  )
  
  new.prevalence.age[, "0-14", ] = prevalence.age[, "0-14", ]
  new.prevalence.age[, "10-19", ] = prevalence.age[, "10-19", ]
  new.prevalence.age[, "15-24", ] = prevalence.age[, "15-24", ]
  new.prevalence.age[, "25-49", ] = new.25.to.49.prevalence
  new.prevalence.age[, "50 and over", ] = prevalence.age[, "50 and over", ]
  
  # lowers
  prevalence.lowers.age = DATA.MANAGER$prevalence.lowers$year.age.location
  # bounds can't be subtracted (lower(15-49) - lower(15-24) is not a lower bound and often lands above the point);
  # instead give 25-49 the same relative interval as 15-49 (fixed 10/15/26)
  new.25.to.49.prevalence.lowers = new.25.to.49.prevalence * prevalence.lowers.age[,"15-49",]/prevalence.age[,"15-49",]
  
  new.prevalence.lowers.age =  array(NA,
                                     dim = c(dim(prevalence.lowers.age)[1], "age"=length(new.ages), dim(prevalence.lowers.age)[3]),
                                     dimnames = list("year"=dimnames(prevalence.lowers.age)[[1]],  # year
                                                     "age"=new.ages,                       # new age categories
                                                     "location"=dimnames(prevalence.lowers.age)[[3]]    # location
                                     )
  )
  
  new.prevalence.lowers.age[, "0-14", ] = prevalence.lowers.age[, "0-14", ]
  new.prevalence.lowers.age[, "10-19", ] = prevalence.lowers.age[, "10-19", ]
  new.prevalence.lowers.age[, "15-24", ] = prevalence.lowers.age[, "15-24", ]
  new.prevalence.lowers.age[, "25-49", ] = new.25.to.49.prevalence.lowers
  new.prevalence.lowers.age[, "50 and over", ] = prevalence.lowers.age[, "50 and over", ]
  
  # uppers
  prevalence.uppers.age = DATA.MANAGER$prevalence.uppers$year.age.location
  # bounds can't be subtracted (lower(15-49) - lower(15-24) is not a lower bound and often lands above the point);
  # instead give 25-49 the same relative interval as 15-49 (fixed 10/15/26)
  new.25.to.49.prevalence.uppers = new.25.to.49.prevalence * prevalence.uppers.age[,"15-49",]/prevalence.age[,"15-49",]
  
  new.prevalence.uppers.age =  array(NA,
                                     dim = c(dim(prevalence.uppers.age)[1], "age"=length(new.ages), dim(prevalence.uppers.age)[3]),
                                     dimnames = list("year"=dimnames(prevalence.uppers.age)[[1]],  # year
                                                     "age"=new.ages,                       # new age categories
                                                     "location"=dimnames(prevalence.uppers.age)[[3]]    # location
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
                                 dim = c(dim(hiv.mortality.age)[1], "age"=length(new.ages), dim(hiv.mortality.age)[3]),
                                 dimnames = list("year"=dimnames(hiv.mortality.age)[[1]],  # year
                                                 "age"=new.ages,                       # new age categories
                                                 "location"=dimnames(hiv.mortality.age)[[3]]    # location
                                 )
  )
  
  new.hiv.mortality.age[, "0-14", ] = hiv.mortality.age[, "0-14", ]
  new.hiv.mortality.age[, "10-19", ] = hiv.mortality.age[, "10-19", ]
  new.hiv.mortality.age[, "15-24", ] = hiv.mortality.age[, "15-24", ]
  new.hiv.mortality.age[, "25-49", ] = new.25.to.49.hiv.mortality
  new.hiv.mortality.age[, "50 and over", ] = hiv.mortality.age[, "50 and over", ]
  
  # lowers
  hiv.mortality.lowers.age = DATA.MANAGER$hiv.mortality.lowers$year.age.location
  # bounds can't be subtracted (lower(15-49) - lower(15-24) is not a lower bound and often lands above the point);
  # instead give 25-49 the same relative interval as 15-49 (fixed 10/15/26)
  new.25.to.49.hiv.mortality.lowers = new.25.to.49.hiv.mortality * hiv.mortality.lowers.age[,"15-49",]/hiv.mortality.age[,"15-49",]
  
  new.hiv.mortality.lowers.age =  array(NA,
                                        dim = c(dim(hiv.mortality.lowers.age)[1], "age"=length(new.ages), dim(hiv.mortality.lowers.age)[3]),
                                        dimnames = list("year"=dimnames(hiv.mortality.lowers.age)[[1]],  # year
                                                        "age"=new.ages,                       # new age categories
                                                        "location"=dimnames(hiv.mortality.lowers.age)[[3]]    # location
                                        )
  )
  
  new.hiv.mortality.lowers.age[, "0-14", ] = hiv.mortality.lowers.age[, "0-14", ]
  new.hiv.mortality.lowers.age[, "10-19", ] = hiv.mortality.lowers.age[, "10-19", ]
  new.hiv.mortality.lowers.age[, "15-24", ] = hiv.mortality.lowers.age[, "15-24", ]
  new.hiv.mortality.lowers.age[, "25-49", ] = new.25.to.49.hiv.mortality.lowers
  new.hiv.mortality.lowers.age[, "50 and over", ] = hiv.mortality.lowers.age[, "50 and over", ]
  
  # uppers
  hiv.mortality.uppers.age = DATA.MANAGER$hiv.mortality.uppers$year.age.location
  # bounds can't be subtracted (lower(15-49) - lower(15-24) is not a lower bound and often lands above the point);
  # instead give 25-49 the same relative interval as 15-49 (fixed 10/15/26)
  new.25.to.49.hiv.mortality.uppers = new.25.to.49.hiv.mortality * hiv.mortality.uppers.age[,"15-49",]/hiv.mortality.age[,"15-49",]
  
  new.hiv.mortality.uppers.age =  array(NA,
                                        dim = c(dim(hiv.mortality.uppers.age)[1], "age"=length(new.ages), dim(hiv.mortality.uppers.age)[3]),
                                        dimnames = list("year"=dimnames(hiv.mortality.uppers.age)[[1]],  # year
                                                        "age"=new.ages,                       # new age categories
                                                        "location"=dimnames(hiv.mortality.uppers.age)[[3]]    # location
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

# BY SEX (10/30/26, UNAIDS AIDSinfo by-sex files): the same new age groups for year.age.sex.location -
# 25-49 = 15-49 - 15-24 within each sex, with 15-49's relative interval for the bounds (as above). No by-sex data for 0-14.
# year.sex.location (all ages by sex) is unchanged.
for(data.type in c("incidence","prevalence","hiv.mortality")){
  old.pt = DATA.MANAGER.OLD[[data.type]]$year.age.sex.location
  if(is.null(old.pt) || !all(c("15-24","15-49") %in% dimnames(old.pt)$age))
    next
  new.25.to.49.by.sex = old.pt[,"15-49",,] - old.pt[,"15-24",,]
  for(suffix in c("", ".lowers", ".uppers")){
    old = DATA.MANAGER.OLD[[paste0(data.type, suffix)]]$year.age.sex.location
    dim.names = dimnames(old)
    dim.names$age = new.ages
    new = array(as.numeric(NA), dim = sapply(dim.names, length), dimnames = dim.names)
    for(age in intersect(c("0-14","10-19","15-24","50 and over"), dimnames(old)$age))
      new[,age,,] = old[,age,,]
    if(suffix=="")
      new[,"25-49",,] = new.25.to.49.by.sex
    else
      new[,"25-49",,] = new.25.to.49.by.sex * old[,"15-49",,]/old.pt[,"15-49",,]
    DATA.MANAGER[[paste0(data.type, suffix)]]$year.age.sex.location = new
  }
}
DATA.MANAGER = fix.count.bounds(DATA.MANAGER, elements = "year.age.sex.location")


# same bounds clean-up as in save_data_manager.R, now on the new 25-49 group (10/15/26)
DATA.MANAGER = fix.count.bounds(DATA.MANAGER, elements = "year.age.location")

save(DATA.MANAGER,file=paste0("cached/data.manager_global_revised_ages_",Sys.Date(),".Rdata"))
