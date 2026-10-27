source('calibration/likelihood/likelihood.R')
source('calibration/mcmc_settings.R')      # MCMC step settings, seeds, Step 1 starting value files
source('calibration/mcmc_start_values.R')  # get.location.prior, get.mcmc.start.values

# Prior sd (log scale) for the 6 mortality slope multipliers, used in every location's prior below.
# A slope multiplier m tilts the fitted non-HIV mortality trend around MORTALITY.SLOPE.ANCHOR.YEAR (2020;
# model/parameter_mappings/calculate_death_rates.R): rate * m^(year - 2020), so it acts over 20 years to
# 2040 and 40 years back to 1980. Allow up to a 4x (or 1/4) change by 2040 at 2 sd: (log(4)/2)/20.
# (It was log(4)/2 with the trend tilted around 1980, which let mortality blow up - e.g., Zambia reached
# a non-HIV mortality rate of ~76,000/year and its model runs became ~100x slower.)
MORTALITY.SLOPE.MULTIPLIER.LOG.SD = (log(4)/2) / 20
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
                                                weighted.cascade = F, # nextgen: if T, 4x awareness/engagement/suppression weights
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
  
    # nextgen option: 4x cascade weights; otherwise create.likelihood's defaults are used
    cascade.weights = list()
    if(weighted.cascade)
        cascade.weights = list(awareness.weight = 4, engagement.weight = 4, suppression.weight = 4)
    # nextgen (10/27/26): down-weight the youth cascade observations (YOUTH.CASCADE.LIKELIHOOD.WEIGHT, calibration/mcmc_settings.R)
    if(location == "South Africa" && !is.null(get0("YOUTH.CASCADE.LIKELIHOOD.WEIGHT")))
        cascade.weights$cascade.weight.by.obs.age = YOUTH.CASCADE.LIKELIHOOD.WEIGHT
    
    if(weighted.prevalence){
      likelihood.to.run = do.call(create.likelihood, c(list(parameters = create.model.parameters(location = location),
                                              location=location,
                                              total.weight = WEIGHTS.BY.YEAR,
                                              population.weight=POP.WEIGHTS*(1/200000), # downweighted due to large pop size/number of strata
                                              prevalence.weight = 4,
                                              total.mortality.weight = total.mortality.weight), cascade.weights))
    } else {
        likelihood.to.run = do.call(create.likelihood, c(list(parameters = create.model.parameters(location = location),
                                              location=location,
                                              total.weight = WEIGHTS.BY.YEAR,
                                              population.weight=POP.WEIGHTS*(1/200000), # downweighted due to large pop size/number of strata
                                              total.mortality.weight = total.mortality.weight), cascade.weights))
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



