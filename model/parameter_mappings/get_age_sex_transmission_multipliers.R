source("model/parameter_mappings/kenya/age_sex_transmission_multipliers_kenya.R")
source ("model/parameter_mappings/south_africa/age_sex_transmission_multipliers_south_africa.R")


get.all.age.sex.transmission.multipliers = function(location){
    # if(location!="Kenya")
    #     print("Using Kenya-specific transmission multipliers by age and sex for now")
    
    rv = list()
    
    # if(location=="Kenya"){
        rv$FEMALE.AGE.MULTIPLIERS.2003 = get.all.transmission.multipliers.kenya(sex="female",year=2003)
        rv$MALE.AGE.MULTIPLIERS.2003 = get.all.transmission.multipliers.kenya(sex="male",year=2003)
        
        rv$FEMALE.AGE.MULTIPLIERS.2008 = get.all.transmission.multipliers.kenya(sex="female",year=2008)
        rv$MALE.AGE.MULTIPLIERS.2008 = get.all.transmission.multipliers.kenya(sex="male",year=2008)
        
        rv$FEMALE.AGE.MULTIPLIERS.2014 = get.all.transmission.multipliers.kenya(sex="female",year=2014)
        rv$MALE.AGE.MULTIPLIERS.2014 = get.all.transmission.multipliers.kenya(sex="male",year=2014)
     
    # } else if(location=="South Africa"){
    #     rv$FEMALE.AGE.MULTIPLIERS.2003 = get.all.transmission.multipliers.south.africa(sex="female",year=2003)
    #     rv$MALE.AGE.MULTIPLIERS.2003 = get.all.transmission.multipliers.south.africa(sex="male",year=2003)
    #     
    #     rv$FEMALE.AGE.MULTIPLIERS.2008 = get.all.transmission.multipliers.south.africa(sex="female",year=2003) # USING 2003 ESTIMATES
    #     rv$MALE.AGE.MULTIPLIERS.2008 = get.all.transmission.multipliers.south.africa(sex="male",year=2003)
    #     
    #     rv$FEMALE.AGE.MULTIPLIERS.2014 = get.all.transmission.multipliers.south.africa(sex="female",year=2016) # USING 2003 ESTIMATES
    #     rv$MALE.AGE.MULTIPLIERS.2014 = get.all.transmission.multipliers.south.africa(sex="male",year=2016)
    # 
    # } else { # using kenya model for all other countries for now 
    #     rv$FEMALE.AGE.MULTIPLIERS.2003 = get.all.transmission.multipliers.kenya(sex="female",year=2003)
    #     rv$MALE.AGE.MULTIPLIERS.2003 = get.all.transmission.multipliers.kenya(sex="male",year=2003)
    #     
    #     rv$FEMALE.AGE.MULTIPLIERS.2008 = get.all.transmission.multipliers.kenya(sex="female",year=2008)
    #     rv$MALE.AGE.MULTIPLIERS.2008 = get.all.transmission.multipliers.kenya(sex="male",year=2008)
    #     
    #     rv$FEMALE.AGE.MULTIPLIERS.2014 = get.all.transmission.multipliers.kenya(sex="female",year=2014)
    #     rv$MALE.AGE.MULTIPLIERS.2014 = get.all.transmission.multipliers.kenya(sex="male",year=2014)
    # 
    # }
        

rv  
}
