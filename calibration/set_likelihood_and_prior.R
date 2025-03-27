source('calibration/likelihood/likelihood.R')
source('calibration/prior_distributions/kenya_prior.R')
source('calibration/prior_distributions/south_africa_prior.R')
source('calibration/prior_distributions/france_prior.R')
source('calibration/prior_distributions/mozambique_prior.R')
source('calibration/prior_distributions/tanzania_prior.R')
source('calibration/prior_distributions/uganda_prior.R')
source('calibration/prior_distributions/zambia_prior.R')
source('calibration/prior_distributions/zimbabwe_prior.R')

set.likelihood.and.prior.by.location = function(location){
    if(location=="Kenya"){
        likelihood.to.run = create.likelihood(parameters = create.model.parameters(location = "Kenya"),
                                              location="Kenya")
        prior = KENYA.PRIOR
        
        load("calibration/starting_values/2025_02_26_kenya_start_values.Rdata")
        params.start.values = params.start.values
        
    } else if(location=="South Africa"){
        likelihood.to.run = create.likelihood(parameters = create.model.parameters(location = "South Africa"),
                                             location="South Africa")
        prior = SOUTH.AFRICA.PRIOR
        
        load("calibration/starting_values/2025_02_26_south_africa_start_values.Rdata")
        params.start.values = params.start.values
        
    } else if(location=="France"){
        likelihood.to.run = create.likelihood(parameters = create.model.parameters(location = "France"),
                                              location="France")
        prior = FRANCE.PRIOR
        
        load("calibration/starting_values/2025_02_26_france_start_values.Rdata")
        params.start.values = params.start.values
        
    } else if(location=="Mozambique"){
        likelihood.to.run = create.likelihood(parameters = create.model.parameters(location = "Mozambique"),
                                              location="Mozambique")
        prior = MOZAMBIQUE.PRIOR
        params.start.values = get.default.parameters(location = "Mozambique")
        params.start.values = params.start.values[prior@var.names]
        
    } else if(location=="Tanzania"){
        likelihood.to.run = create.likelihood(parameters = create.model.parameters(location = "Tanzania"),
                                              location="Tanzania")
        prior = TANZANIA.PRIOR
        params.start.values = get.default.parameters(location = "Tanzania")
        params.start.values = params.start.values[prior@var.names]
        
    } else if(location=="Uganda"){
        likelihood.to.run = create.likelihood(parameters = create.model.parameters(location = "Uganda"),
                                              location="Uganda")
        prior = UGANDA.PRIOR
        params.start.values = get.default.parameters(location = "Uganda")
        params.start.values = params.start.values[prior@var.names]
        
    } else if(location=="Zambia"){
        likelihood.to.run = create.likelihood(parameters = create.model.parameters(location = "Zambia"),
                                              location="Zambia")
        prior = ZAMBIA.PRIOR
        params.start.values = get.default.parameters(location = "Zambia")
        params.start.values = params.start.values[prior@var.names]
        
    } else if(location=="Zimbabwe"){
        likelihood.to.run = create.likelihood(parameters = create.model.parameters(location = "Zimbabwe"),
                                              location="Zimbabwe")
        prior = ZIMBABWE.PRIOR
        params.start.values = get.default.parameters(location = "Zimbabwe")
        params.start.values = params.start.values[prior@var.names]
        
    } else stop("Only set up for Kenya, South Africa, France, Mozambique, Tanzania, Uganda, Zambia, Zimbabwe for now")
    
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



