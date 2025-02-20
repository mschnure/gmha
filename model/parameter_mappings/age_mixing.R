######################################################################################
# Description: Code to return regression coefficients for mean partner age differences
######################################################################################

source("model/parameter_mappings/kenya/age_mixing_kenya_south_africa.R")
# 1. Using data on female ages and the mean difference in age of their partner, regresses on both the mean 
#     difference and the standard deviation of the mean differences, returns intercept and slope for each
# 2. Uses the data exactly from the paper; HIV positive women in male-female clusters with HIV-positive male; 
#     presumed male to female transmission 

# Must be sourced before parameters

# Functions 
#     1. get.male.to.female.age.model
#     2. get.female.to.male.age.model


get.male.to.female.age.model = function(location){
    # if(location!="Kenya")
    #     print("Using Kenya-specific male-to-female sexual partnerships by age for now")
    
    rv = get.male.to.female.age.model.kenya.south.africa() # paper is from south africa, using it for both Kenya/SA
    rv
}
get.female.to.male.age.model = function(location){
    # if(location!="Kenya")
    #     print("Using Kenya-specific female-to-male sexual partnerships by age for now")
    
    rv = get.female.to.male.age.model.kenya.south.africa() # paper is from south africa, using it for both Kenya/SA
    rv
}


