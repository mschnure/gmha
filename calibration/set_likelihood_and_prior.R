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

set.likelihood.and.prior.by.location = function(location){
    likelihood.to.run = create.likelihood(parameters = create.model.parameters(location = location),
                                          location=location)
    
    params.start.values = get.default.parameters(location = location)
    params.start.values = params.start.values[prior@var.names]
    
    if(location=="Kenya"){
        prior = KENYA.PRIOR
        load("calibration/starting_values/2025_02_26_kenya_start_values.Rdata")
        params.start.values = params.start.values
    } else if(location=="South Africa"){
        prior = SOUTH.AFRICA.PRIOR
        load("calibration/starting_values/2025_02_26_south_africa_start_values.Rdata")
        params.start.values = params.start.values
    } else if(location=="France"){
        prior = FRANCE.PRIOR
        load("calibration/starting_values/2025_02_26_france_start_values.Rdata")
        params.start.values = params.start.values
    } else if(location=="Mozambique"){
        prior = MOZAMBIQUE.PRIOR
    } else if(location=="Tanzania"){
        prior = TANZANIA.PRIOR
    } else if(location=="Uganda"){
        prior = UGANDA.PRIOR
    } else if(location=="Zambia"){
        prior = ZAMBIA.PRIOR
    } else if(location=="Zimbabwe"){
        prior = ZIMBABWE.PRIOR
    } else if(location=="Malawi"){
        prior = MALAWI.PRIOR
    } else 
        stop("Only set up for Kenya, South Africa, France, Mozambique, Tanzania, Uganda, Zambia, Zimbabwe, Malawi for now")
    
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



