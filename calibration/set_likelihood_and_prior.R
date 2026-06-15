source('calibration/likelihood/likelihood.R')
source('calibration/prior_distributions/kenya_prior.R')
source('calibration/prior_distributions/south_africa_prior.R')
source('calibration/prior_distributions/france_prior.R')
source('calibration/prior_distributions/mozambique_prior.R')
source('calibration/prior_distributions/tanzania_prior.R')
source('calibration/prior_distributions/uganda_prior.R')
source('calibration/prior_distributions/zambia_prior.R')
source('calibration/prior_distributions/zimbabwe_prior.R')
source('calibration/prior_distributions/malawi_prior.R')
source('calibration/prior_distributions/nigeria_prior.R')
source('calibration/prior_distributions/unaids_remainder_prior.R')
source('calibration/prior_distributions/non_unaids_remainder_prior.R')
source('calibration/prior_distributions/thailand_prior.R')
source('calibration/prior_distributions/cambodia_prior.R')
source("calibration/prior_distributions/r1_low_prior.R")
source("calibration/prior_distributions/r1_lower_middle_prior.R")
source("calibration/prior_distributions/r1_upper_middle_prior.R")
source("calibration/prior_distributions/r1_high_prior.R")

set.likelihood.and.prior.by.location = function(location,
                                                total.weight = 1,
                                                total.mortality.weight = 1/1000, # added 6/2 to allow third step to have weight = 1
                                                weighted.prevalence = F){
  
  WEIGHT.YEARS = 1970:2030
  WEIGHTS.BY.YEAR = rep(1, length(WEIGHT.YEARS))
  WEIGHTS.BY.YEAR = (1/4)^(WEIGHT.YEARS<2000) # before 2000, 1/4x
  WEIGHTS.BY.YEAR[WEIGHT.YEARS>=2018] = 4 # from 2018, 4x
  names(WEIGHTS.BY.YEAR) = WEIGHT.YEARS
  WEIGHTS.BY.YEAR = WEIGHTS.BY.YEAR*total.weight
  
  # Population data extended through 2040
  POP.WEIGHTS = c(WEIGHTS.BY.YEAR,rep(total.weight,10))
  names(POP.WEIGHTS) = c(names(WEIGHTS.BY.YEAR),2031:2040)
  
    if(weighted.prevalence){
        likelihood.to.run = create.likelihood(parameters = create.model.parameters(location = location),
                                              location=location,
                                              total.weight = WEIGHTS.BY.YEAR,
                                              population.weight=POP.WEIGHTS*(1/200000), # downweighted due to large pop size/number of strata
                                              prevalence.weight = 4,
                                              total.mortality.weight = total.mortality.weight)
    } else {
        likelihood.to.run = create.likelihood(parameters = create.model.parameters(location = location),
                                              location=location,
                                              total.weight = WEIGHTS.BY.YEAR,
                                              population.weight=POP.WEIGHTS*(1/200000), # downweighted due to large pop size/number of strata
                                              total.mortality.weight = total.mortality.weight)
                                              }

    print("STARTING FROM PRIORS, NOT MANUAL VALUES - MUST BE INITIAL RUN")
    #print("STARTING FROM 5/29 STARTING VALUES")
    #print("STARTING FROM 6/2 STARTING VALUES")
    if(location=="Kenya"){
      prior = KENYA.PRIOR
      #params.start.values = get.medians(prior)
      #load("calibration/starting_values/2026-05-29_kenya_start_values.Rdata")
      load("calibration/starting_values/2026-06-02_kenya_start_values.Rdata")
    } else if(location=="South Africa"){
      prior = SOUTH.AFRICA.PRIOR
      #params.start.values = get.medians(prior)
      #load("calibration/starting_values/2026-05-29_south_africa_start_values.Rdata")
      load("calibration/starting_values/2026-06-02_south_africa_start_values.Rdata")
    } else if(location=="Mozambique"){
      prior = MOZAMBIQUE.PRIOR
      #params.start.values = get.medians(prior)
      #load("calibration/starting_values/2026-05-29_mozambique_start_values.Rdata")
      load("calibration/starting_values/2026-06-02_mozambique_start_values.Rdata")
    } else if(location=="Tanzania"){
      prior = TANZANIA.PRIOR
      #params.start.values = get.medians(prior)
      #load("calibration/starting_values/2026-05-29_tanzania_start_values.Rdata")
      load("calibration/starting_values/2026-06-02_tanzania_start_values.Rdata")
    } else if(location=="Uganda"){
      prior = UGANDA.PRIOR
      #params.start.values = get.medians(prior)
      #load("calibration/starting_values/2026-05-29_uganda_start_values.Rdata")
      load("calibration/starting_values/2026-06-02_uganda_start_values.Rdata")
    } else if(location=="Zambia"){
      prior = ZAMBIA.PRIOR
      #params.start.values = get.medians(prior)
      #load("calibration/starting_values/2026-05-29_zambia_start_values.Rdata")
      load("calibration/starting_values/2026-06-02_zambia_start_values.Rdata")
    } else if(location=="Zimbabwe"){
      prior = ZIMBABWE.PRIOR
      #params.start.values = get.medians(prior)
      #load("calibration/starting_values/2026-05-29_zimbabwe_start_values.Rdata")
      load("calibration/starting_values/2026-06-03_zimbabwe_start_values.Rdata")
    } else if(location=="Malawi"){
      prior = MALAWI.PRIOR
      #params.start.values = get.medians(prior)
      #load("calibration/starting_values/2026-05-29_malawi_start_values.Rdata")
      load("calibration/starting_values/2026-06-02_malawi_start_values.Rdata")
    } else if(location=="Nigeria"){
      prior = NIGERIA.PRIOR
      #params.start.values = get.medians(prior)
      #load("calibration/starting_values/2026-05-29_nigeria_start_values.Rdata")
      load("calibration/starting_values/2026-06-03_nigeria_start_values.Rdata")
    } else if(location=="unaids.remainder"){
      prior = UNAIDS.REMAINDER.PRIOR
      params.start.values = get.medians(prior)
      #load("calibration/starting_values/2026-05-29_unaids.remainder_start_values.Rdata")
      #load("calibration/starting_values/2026-06-02_unaids.remainder_start_values.Rdata")
    } else if(location=="non.unaids.remainder"){
      prior = NON.UNAIDS.REMAINDER.PRIOR
      params.start.values = get.medians(prior)
      #load("calibration/starting_values/2026-05-29_non.unaids.remainder_start_values.Rdata")
      #load("calibration/starting_values/2026-06-02_non.unaids.remainder_start_values.Rdata")
    } else if(location=="r1.low"){
      prior = R1.LOW.PRIOR
      params.start.values = get.medians(prior)
      #load("calibration/starting_values/2026-05-29_r1_low_start_values.Rdata")
      #load("calibration/starting_values/2026-06-02_r1_low_start_values.Rdata")
    } else if(location=="r1.lower.middle"){
      prior = R1.LOWER.MIDDLE.PRIOR
      params.start.values = get.medians(prior)
      #load("calibration/starting_values/2026-05-29_r1_lower.middle_start_values.Rdata")
      #load("calibration/starting_values/2026-06-02_r1_lower.middle_start_values.Rdata")
    } else if(location=="r1.upper.middle"){
      prior = R1.UPPER.MIDDLE.PRIOR
      params.start.values = get.medians(prior)
      #load("calibration/starting_values/2026-05-29_r1_upper.middle_start_values.Rdata")
      #load("calibration/starting_values/2026-06-02_r1_upper.middle_start_values.Rdata")
    } else if(location == "r1.high"){
      prior = R1.HIGH.PRIOR
      params.start.values = get.medians(prior)
      #load("calibration/starting_values/2026-05-29_r1_high_start_values.Rdata")
      #load("calibration/starting_values/2026-06-02_r1_high_start_values.Rdata")
      
      
    } else stop("Only set up for Kenya, South Africa, Mozambique, Tanzania, 
             Uganda, Zambia, Zimbabwe, Malawi, Nigeria, and remainder models for now")
    
    params.start.values = params.start.values[prior@var.names]
    
    transformations = unlist(sapply(prior@subdistributions,function(dist){
        
        if(.hasSlot(dist,"transformations"))
            sapply(dist@transformations,function(tf){
                tf@name
            })
        else if(is.null(dist@transformation))
            "identity"
        else
            dist@transformation@name
        
    }))
    
    names(transformations) = prior@var.names
    
    sds = get.sds(prior)
    sds = sds[names(params.start.values)]
    
    rv = list()
    rv$likelihood.to.run = likelihood.to.run
    rv$prior = prior
    rv$params.start.values = params.start.values
    rv$transformations = transformations
    rv$sds = sds
    
    rv
    
} 



