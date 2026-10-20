source("model/run_systematic.R")
source('calibration/prior_distributions/south_africa_prior.R')
prior = SOUTH.AFRICA.PRIOR
# params.start.values = params.start.values[prior@var.names]

# youth suppression calibration
if(1==1){
  # initial run of 30,000 with 1/8 weight
  #load("mcmc_runs/simset_south_africa_youth.supp_1_youth.supp_chain1_2025-10-08.Rdata")
  # after that ^ initial run, running for 30,000 with 4x prevalence weight
  
  # second run with 4x prevalence weight & cascade weight
  load("mcmc_runs/simset_south_africa_youth.supp_1_youth.supp_chain1_2025-10-23.Rdata")
}

#original calibration
if(1==2){
  # initial run of 100,000 with 1/8 weight
  #load("mcmc_runs/simset_south_africa_2025-07-30.Rdata")
  # after that ^ initial run, ran 50,000 with 4x prevalence weight
  load("mcmc_runs/simset_south_africa_2025-08-10.Rdata")
}


default.params =  get.default.parameters(location = "South Africa") 
params.start.values = simset@parameters[simset@n.sim,] 
additional.params = setdiff(names(default.params),names(params.start.values))

# youth suppression calibration 
if(1==1){
  #save(params.start.values,file=("calibration/starting_values/2025_10_08_south_africa_start_values.Rdata"))  
  save(params.start.values,file=("calibration/starting_values/2025_10_23_south_africa_start_values.Rdata"))  
}

if(1==2){
  #save(params.start.values,file=("calibration/starting_values/2025_08_04_south_africa_start_values.Rdata"))
  save(params.start.values,file=("calibration/starting_values/2025_08_11_south_africa_start_values.Rdata"))  
}
