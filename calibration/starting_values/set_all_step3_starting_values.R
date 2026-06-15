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

COUNTRIES = c(#"Kenya","Malawi","Mozambique",
              "Nigeria"#,
              #"South Africa","Tanzania","Uganda","Zambia",
              #"Zimbabwe"#,
              #"unaids.remainder","non.unaids.remainder",
              #"r1.low","r1.lower.middle","r1.upper.middle","r1.high"
              )

for(country in COUNTRIES){
  print(paste0("processing ",country))
  
  prior.name = paste0(convert_string_upper_periods(country),".PRIOR")
  print(paste0("loading ",prior.name))
  prior = get(prior.name)
  
  if(country %in% c("non.unaids.remainder","r1.lower.middle","unaids.remainder","r1.upper.middle")){
    RUN.DATE = "05-30"
  } else if (country %in% c("Zambia")){
    RUN.DATE = "06-01"  
  } else if (country %in% c("Nigeria","Zimbabwe")){
    RUN.DATE = "06-03"  
  } else {
    RUN.DATE = "05-31"
  }
  
  # previous run of 50,000 with 4x prevalence weight
  file = paste0("mcmc_runs/simset_",convert_string(country),"_chain1_2026-",RUN.DATE,".Rdata")
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

