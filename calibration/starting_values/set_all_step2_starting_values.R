source("model/run_systematic.R")
source('calibration/prior_distributions/kenya_prior.R')
source('calibration/prior_distributions/south_africa_prior.R')
source('calibration/prior_distributions/mozambique_prior.R')
source('calibration/prior_distributions/tanzania_prior.R')
source('calibration/prior_distributions/uganda_prior.R')
source('calibration/prior_distributions/zambia_prior.R')
source('calibration/prior_distributions/zimbabwe_prior.R')
source('calibration/prior_distributions/malawi_prior.R')
source('calibration/prior_distributions/nigeria_prior.R')
source('calibration/prior_distributions/unaids_remainder_prior.R')
source('calibration/prior_distributions/non_unaids_remainder_prior.R')
source("calibration/prior_distributions/r1_low_prior.R")
source("calibration/prior_distributions/r1_lower_middle_prior.R")
source("calibration/prior_distributions/r1_upper_middle_prior.R")
source("calibration/prior_distributions/r1_high_prior.R")

COUNTRIES = c("Kenya","Malawi","Mozambique","Nigeria","South Africa","Tanzania","Uganda","Zambia","Zimbabwe",
              "unaids.remainder","non.unaids.remainder",
              "r1.low", "r1.lower.middle","r1.upper.middle","r1.high")

for(country in COUNTRIES){
  print(paste0("processing ",country))
  
  prior.name = paste0(convert_string_upper_periods(country),".PRIOR")
  print(paste0("loading ",prior.name))
  prior = get(prior.name)
  
  if(country=="r1.lower.middle"){
    RUN.DATE = "7"
  } else if (country %in% c("Nigeria","Zambia")){
    RUN.DATE = "9"  
  } else {
    RUN.DATE = "8"
  }
  
  # initial run of 50,000 with 1/8 weight
  file = paste0("mcmc_runs/simset_",convert_string(country),"_chain1_2026-05-2",RUN.DATE,".Rdata")
  if(country %in% c("r1.low","r1.lower.middle","r1.upper.middle","r1.high",
                    "unaids.remainder","non.unaids.remainder"))
    file = paste0("mcmc_runs/simset_",convert_string(country),"_chain1_2026-06-17.Rdata")
  print(paste0("loading file: ",file))
  load(file)
  
  default.params =  get.medians(prior)
  params.start.values = simset@parameters[simset@n.sim,] 
  additional.params = setdiff(names(default.params),names(params.start.values))
  
  save.file.name = paste0("calibration/starting_values/",Sys.Date(),"_",convert_string(country),"_start_values.Rdata")
  save.file.name = gsub("r1.","r1_",save.file.name)
  
  print(paste0("saving file: ",save.file.name))
  save(params.start.values,
       file=save.file.name)
  }

