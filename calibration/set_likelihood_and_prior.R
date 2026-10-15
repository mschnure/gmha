source('calibration/likelihood/likelihood.R')
source('calibration/mcmc_settings.R')      # MCMC step settings, seeds, Step 1 starting value files
source('calibration/mcmc_start_values.R')  # get.location.prior, get.mcmc.start.values
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
                                                weighted.prevalence = F,
                                                step = 2, # which MCMC step's starting values to use (see calibration/mcmc_start_values.R)
                                                params.start.values = NULL){ # or pass starting values directly
  
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

    # prior for this location, and starting values for this MCMC step:
    #   step 1 -> prior medians, or the file in STEP1.START.VALUE.FILES (calibration/mcmc_settings.R)
    #   step 2 -> most recent Step 2 starting values file (saved automatically at the end of Step 1)
    prior = get.location.prior(location)
    if (is.null(params.start.values))
        params.start.values = get.mcmc.start.values(location, step = step, prior = prior)
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



