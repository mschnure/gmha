source('calibration/likelihood/likelihood.R')
source('calibration/prior_distributions/kenya_prior.R')
source('calibration/prior_distributions/south_africa_prior.R')
source('calibration/prior_distributions/france_prior.R')

set.likelihood.and.prior.by.location = function(location){
    if(location=="Kenya"){
        likelihood.to.run = create.likelihood(parameters = create.model.parameters(location = "Kenya"),
                                      location="Kenya")
        prior = KENYA.PRIOR
        
        load("calibration/starting_values/2025_01_14_kenya_start_values.Rdata") 
        params.start.values = params.start.values
        
    } else if(location=="South Africa"){
        likelihood.to.run = create.likelihood(parameters = create.model.parameters(location = "South Africa"),
                                             location="South Africa")
        prior = SOUTH.AFRICA.PRIOR
        
        load("calibration/starting_values/2025_01_14_south_africa_start_values.Rdata") 
        params.start.values = params.start.values
        
    } else if(location=="France"){
        likelihood.to.run = create.likelihood(parameters = create.model.parameters(location = "France"),
                                       location="France")
        
        prior = FRANCE.PRIOR
        
        load("calibration/starting_values/2025_01_14_france_start_values.Rdata") 
        params.start.values = params.start.values
        
    } else stop("Only set up for Kenya, South Africa, and France for now")
    
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



