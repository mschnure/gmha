# source('calibration/parameter_mappings/prior_distributions.R')

# PRIOR is set to country-specific prior in prior_distributions.R

# loads params.start.values object 
load("calibration/starting_values/2025_01_10_kenya_start_values.Rdata") 
PARAMS.START.VALUES.KENYA = params.start.values

load("calibration/starting_values/2025_01_10_south_africa_start_values.Rdata") 
PARAMS.START.VALUES.SOUTH.AFRICA = params.start.values

load("calibration/starting_values/2025_01_10_france_start_values.Rdata") 
PARAMS.START.VALUES.FRANCE = params.start.values

TRANSFORMATIONS = unlist(sapply(PRIOR@subdistributions,function(dist){
    
    if(.hasSlot(dist,"transformations"))
        sapply(dist@transformations,function(tf){
            tf@name
        })
    else if(is.null(dist@transformation))
        "identity"
    else
        dist@transformation@name
    
}))

names(TRANSFORMATIONS) = PRIOR@var.names

SDS = get.sds(PRIOR)
SDS = SDS[names(PARAMS.START.VALUES)]
