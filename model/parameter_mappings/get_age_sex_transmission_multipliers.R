source("model/parameter_mappings/kenya/age_sex_transmission_multipliers_kenya.R")
source("model/parameter_mappings/south_africa/age_sex_transmission_multipliers_south_africa.R")
source("model/parameter_mappings/france/age_sex_transmission_multipliers_france.R")
source("model/parameter_mappings/mozambique/age_sex_transmission_multipliers_mozambique.R")
source("model/parameter_mappings/tanzania/age_sex_transmission_multipliers_tanzania.R")


get.all.age.sex.transmission.multipliers = function(location){
    # if(location!="Kenya")
    #     print("Using Kenya-specific transmission multipliers by age and sex for now")
    
    rv = list()
    
    if(location=="Kenya"){
        rv$FEMALE.AGE.MULTIPLIERS.2003 = get.all.transmission.multipliers.kenya(sex="female",year=2003)
        rv$MALE.AGE.MULTIPLIERS.2003 = get.all.transmission.multipliers.kenya(sex="male",year=2003)
        
        rv$FEMALE.AGE.MULTIPLIERS.2008 = get.all.transmission.multipliers.kenya(sex="female",year=2008)
        rv$MALE.AGE.MULTIPLIERS.2008 = get.all.transmission.multipliers.kenya(sex="male",year=2008)
        
        rv$FEMALE.AGE.MULTIPLIERS.2014 = get.all.transmission.multipliers.kenya(sex="female",year=2014)
        rv$MALE.AGE.MULTIPLIERS.2014 = get.all.transmission.multipliers.kenya(sex="male",year=2014)
        
    } else if(location=="South Africa"){
        rv$FEMALE.AGE.MULTIPLIERS.2003 = get.all.transmission.multipliers.south.africa(sex="female",year=2003)
        rv$MALE.AGE.MULTIPLIERS.2003 = get.all.transmission.multipliers.south.africa(sex="male",year=2003)
        
        rv$FEMALE.AGE.MULTIPLIERS.2008 = get.all.transmission.multipliers.south.africa(sex="female",year=2003) # USING 2003 ESTIMATES
        rv$MALE.AGE.MULTIPLIERS.2008 = get.all.transmission.multipliers.south.africa(sex="male",year=2003)
        
        rv$FEMALE.AGE.MULTIPLIERS.2014 = get.all.transmission.multipliers.south.africa(sex="female",year=2016) # USING 2016 ESTIMATES
        rv$MALE.AGE.MULTIPLIERS.2014 = get.all.transmission.multipliers.south.africa(sex="male",year=2016)
        
    } else if(location=="France"){
        rv$FEMALE.AGE.MULTIPLIERS.2003 = get.all.transmission.multipliers.france(sex="female",year=2007) # USING 2007 ESTIMATES
        rv$MALE.AGE.MULTIPLIERS.2003 = get.all.transmission.multipliers.france(sex="male",year=2007)
        
        rv$FEMALE.AGE.MULTIPLIERS.2008 = get.all.transmission.multipliers.france(sex="female",year=2007)
        rv$MALE.AGE.MULTIPLIERS.2008 = get.all.transmission.multipliers.france(sex="male",year=2007)
        
        rv$FEMALE.AGE.MULTIPLIERS.2014 = get.all.transmission.multipliers.france(sex="female",year=2023)
        rv$MALE.AGE.MULTIPLIERS.2014 = get.all.transmission.multipliers.france(sex="male",year=2023)
        
    } else if(location=="Mozambique"){
        rv$FEMALE.AGE.MULTIPLIERS.2003 = get.all.transmission.multipliers.mozambique(sex="female",year=2003)
        rv$MALE.AGE.MULTIPLIERS.2003 = get.all.transmission.multipliers.mozambique(sex="male",year=2003)
        
        rv$FEMALE.AGE.MULTIPLIERS.2008 = get.all.transmission.multipliers.mozambique(sex="female",year=2011) # USING 2011 ESTIMATES
        rv$MALE.AGE.MULTIPLIERS.2008 = get.all.transmission.multipliers.mozambique(sex="male",year=2011)
        
        rv$FEMALE.AGE.MULTIPLIERS.2014 = get.all.transmission.multipliers.mozambique(sex="female",year=2022) # USING 2022 ESTIMATES
        rv$MALE.AGE.MULTIPLIERS.2014 = get.all.transmission.multipliers.mozambique(sex="male",year=2022)
        
    } else if(location=="Tanzania"){
        rv$FEMALE.AGE.MULTIPLIERS.2003 = get.all.transmission.multipliers.tanzania(sex="female",year=2004)
        rv$MALE.AGE.MULTIPLIERS.2003 = get.all.transmission.multipliers.tanzania(sex="male",year=2004)
        
        rv$FEMALE.AGE.MULTIPLIERS.2008 = get.all.transmission.multipliers.tanzania(sex="female",year=2010) # USING 2010 ESTIMATES
        rv$MALE.AGE.MULTIPLIERS.2008 = get.all.transmission.multipliers.tanzania(sex="male",year=2010)
        
        rv$FEMALE.AGE.MULTIPLIERS.2014 = get.all.transmission.multipliers.tanzania(sex="female",year=2022) # USING 2022 ESTIMATES
        rv$MALE.AGE.MULTIPLIERS.2014 = get.all.transmission.multipliers.tanzania(sex="male",year=2022)
        
    } else { # using kenya model for all other countries for now
        rv$FEMALE.AGE.MULTIPLIERS.2003 = get.all.transmission.multipliers.kenya(sex="female",year=2003)
        rv$MALE.AGE.MULTIPLIERS.2003 = get.all.transmission.multipliers.kenya(sex="male",year=2003)
        
        rv$FEMALE.AGE.MULTIPLIERS.2008 = get.all.transmission.multipliers.kenya(sex="female",year=2008)
        rv$MALE.AGE.MULTIPLIERS.2008 = get.all.transmission.multipliers.kenya(sex="male",year=2008)
        
        rv$FEMALE.AGE.MULTIPLIERS.2014 = get.all.transmission.multipliers.kenya(sex="female",year=2014)
        rv$MALE.AGE.MULTIPLIERS.2014 = get.all.transmission.multipliers.kenya(sex="male",year=2014)
        
    }
    
    
    rv  
}
