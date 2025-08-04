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

set.likelihood.and.prior.by.location = function(location){
    likelihood.to.run = create.likelihood(parameters = create.model.parameters(location = location),
                                          location=location)
    
    params.start.values = get.default.parameters(location = location)
    
    if(location=="Kenya"){
        prior = KENYA.PRIOR
        load("calibration/starting_values/2025_08_04_kenya_start_values.Rdata")
        params.start.values = params.start.values
    } else if(location=="South Africa"){
        prior = SOUTH.AFRICA.PRIOR
        load("calibration/starting_values/2025_08_04_south_africa_start_values.Rdata")
        params.start.values = params.start.values
    } else if(location=="France"){
        prior = FRANCE.PRIOR
        load("calibration/starting_values/2025_08_04_france_start_values.Rdata")
        params.start.values = params.start.values
    } else if(location=="Mozambique"){
        prior = MOZAMBIQUE.PRIOR
        load("calibration/starting_values/2025_08_04_mozambique_start_values.Rdata")
        params.start.values = params.start.values
    } else if(location=="Tanzania"){
        prior = TANZANIA.PRIOR
        load("calibration/starting_values/2025_08_04_tanzania_start_values.Rdata")
        params.start.values = params.start.values
    } else if(location=="Uganda"){
        prior = UGANDA.PRIOR
        load("calibration/starting_values/2025_08_04_uganda_start_values.Rdata")
        params.start.values = params.start.values
    } else if(location=="Zambia"){
        prior = ZAMBIA.PRIOR
        load("calibration/starting_values/2025_08_04_zambia_start_values.Rdata")
        params.start.values = params.start.values
    } else if(location=="Zimbabwe"){
        prior = ZIMBABWE.PRIOR
        load("calibration/starting_values/2025_08_04_zimbabwe_start_values.Rdata")
        params.start.values = params.start.values
    } else if(location=="Malawi"){
        prior = MALAWI.PRIOR
        load("calibration/starting_values/2025_08_04_malawi_start_values.Rdata")
        params.start.values = params.start.values
    } else if(location=="Nigeria"){
        prior = NIGERIA.PRIOR
        load("calibration/starting_values/2025_08_04_nigeria_start_values.Rdata")
        params.start.values = params.start.values
    } else if(location=="Thailand"){
        prior = THAILAND.PRIOR
        load("calibration/starting_values/2025_08_04_thailand_start_values.Rdata")
        params.start.values = params.start.values
    } else if(location=="Cambodia"){
        prior = CAMBODIA.PRIOR
        load("calibration/starting_values/2025_08_04_cambodia_start_values.Rdata")
        params.start.values = params.start.values
    } else if(location=="unaids.remainder"){
        prior = UNAIDS.REMAINDER.PRIOR
    } else if(location=="non.unaids.remainder"){
        prior = NON.UNAIDS.REMAINDER.PRIOR
    } else if(location %in% c("r1.low","r1.lower.middle",
                                  "r1.upper.middle","r1.high")){
        prior = UNAIDS.REMAINDER.PRIOR
        print("using UNAIDS remainder prior for all r1 models")
    } else stop("Only set up for Kenya, South Africa, France, Mozambique, Tanzania, 
             Uganda, Zambia, Zimbabwe, Malawi, Nigeria, Thailand, Cambodia, and remainder models for now")
    
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



