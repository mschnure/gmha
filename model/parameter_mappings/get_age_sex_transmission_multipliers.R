source("model/parameter_mappings/kenya/age_sex_transmission_multipliers_kenya.R")
source("model/parameter_mappings/south_africa/age_sex_transmission_multipliers_south_africa.R")
source("model/parameter_mappings/france/age_sex_transmission_multipliers_france.R")
source("model/parameter_mappings/mozambique/age_sex_transmission_multipliers_mozambique.R")
source("model/parameter_mappings/tanzania/age_sex_transmission_multipliers_tanzania.R")
source("model/parameter_mappings/uganda/age_sex_transmission_multipliers_uganda.R")
source("model/parameter_mappings/zambia/age_sex_transmission_multipliers_zambia.R")
source("model/parameter_mappings/zimbabwe/age_sex_transmission_multipliers_zimbabwe.R")
source("model/parameter_mappings/malawi/age_sex_transmission_multipliers_malawi.R")
source("model/parameter_mappings/nigeria/age_sex_transmission_multipliers_nigeria.R")
source("model/parameter_mappings/non_unaids_remainder/age_sex_transmission_multipliers_india.R")
source("model/parameter_mappings/unaids_remainder/age_sex_transmission_multipliers_unaids.R")
source("model/parameter_mappings/thailand/age_sex_transmission_multipliers_thailand.R")


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
        
        rv$FEMALE.AGE.MULTIPLIERS.2014 = get.all.transmission.multipliers.south.africa(sex="female",year=2016)
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
        
        rv$FEMALE.AGE.MULTIPLIERS.2008 = get.all.transmission.multipliers.mozambique(sex="female",year=2011) 
        rv$MALE.AGE.MULTIPLIERS.2008 = get.all.transmission.multipliers.mozambique(sex="male",year=2011)
        
        rv$FEMALE.AGE.MULTIPLIERS.2014 = get.all.transmission.multipliers.mozambique(sex="female",year=2022) 
        rv$MALE.AGE.MULTIPLIERS.2014 = get.all.transmission.multipliers.mozambique(sex="male",year=2022)
        
    } else if(location=="Tanzania"){
        rv$FEMALE.AGE.MULTIPLIERS.2003 = get.all.transmission.multipliers.tanzania(sex="female",year=2004)
        rv$MALE.AGE.MULTIPLIERS.2003 = get.all.transmission.multipliers.tanzania(sex="male",year=2004)
        
        rv$FEMALE.AGE.MULTIPLIERS.2008 = get.all.transmission.multipliers.tanzania(sex="female",year=2010) 
        rv$MALE.AGE.MULTIPLIERS.2008 = get.all.transmission.multipliers.tanzania(sex="male",year=2010)
        
        rv$FEMALE.AGE.MULTIPLIERS.2014 = get.all.transmission.multipliers.tanzania(sex="female",year=2022) 
        rv$MALE.AGE.MULTIPLIERS.2014 = get.all.transmission.multipliers.tanzania(sex="male",year=2022)
        
    } else if(location=="Uganda"){
        rv$FEMALE.AGE.MULTIPLIERS.2003 = get.all.transmission.multipliers.uganda(sex="female",year=2000)
        rv$MALE.AGE.MULTIPLIERS.2003 = get.all.transmission.multipliers.uganda(sex="male",year=2000)
        
        rv$FEMALE.AGE.MULTIPLIERS.2008 = get.all.transmission.multipliers.uganda(sex="female",year=2006)
        rv$MALE.AGE.MULTIPLIERS.2008 = get.all.transmission.multipliers.uganda(sex="male",year=2006)
        
        rv$FEMALE.AGE.MULTIPLIERS.2014 = get.all.transmission.multipliers.uganda(sex="female",year=2016)
        rv$MALE.AGE.MULTIPLIERS.2014 = get.all.transmission.multipliers.uganda(sex="male",year=2016)
        
    } else if(location=="Zambia"){
        rv$FEMALE.AGE.MULTIPLIERS.2003 = get.all.transmission.multipliers.zambia(sex="female",year=2001)
        rv$MALE.AGE.MULTIPLIERS.2003 = get.all.transmission.multipliers.zambia(sex="male",year=2001)
        
        rv$FEMALE.AGE.MULTIPLIERS.2008 = get.all.transmission.multipliers.zambia(sex="female",year=2007)
        rv$MALE.AGE.MULTIPLIERS.2008 = get.all.transmission.multipliers.zambia(sex="male",year=2007)
        
        rv$FEMALE.AGE.MULTIPLIERS.2014 = get.all.transmission.multipliers.zambia(sex="female",year=2018)
        rv$MALE.AGE.MULTIPLIERS.2014 = get.all.transmission.multipliers.zambia(sex="male",year=2018)
        
    } else if(location=="Zimbabwe"){
        rv$FEMALE.AGE.MULTIPLIERS.2003 = get.all.transmission.multipliers.zimbabwe(sex="female",year=2005) 
        rv$MALE.AGE.MULTIPLIERS.2003 = get.all.transmission.multipliers.zimbabwe(sex="male",year=2005)
        
        rv$FEMALE.AGE.MULTIPLIERS.2008 = get.all.transmission.multipliers.zimbabwe(sex="female",year=2010) 
        rv$MALE.AGE.MULTIPLIERS.2008 = get.all.transmission.multipliers.zimbabwe(sex="male",year=2010)
        
        rv$FEMALE.AGE.MULTIPLIERS.2014 = get.all.transmission.multipliers.zimbabwe(sex="female",year=2015) 
        rv$MALE.AGE.MULTIPLIERS.2014 = get.all.transmission.multipliers.zimbabwe(sex="male",year=2015)
        
    } else if(location=="Malawi"){
        rv$FEMALE.AGE.MULTIPLIERS.2003 = get.all.transmission.multipliers.malawi(sex="female",year=2004) 
        rv$MALE.AGE.MULTIPLIERS.2003 = get.all.transmission.multipliers.malawi(sex="male",year=2004)
        
        rv$FEMALE.AGE.MULTIPLIERS.2008 = get.all.transmission.multipliers.malawi(sex="female",year=2010) 
        rv$MALE.AGE.MULTIPLIERS.2008 = get.all.transmission.multipliers.malawi(sex="male",year=2010)
        
        rv$FEMALE.AGE.MULTIPLIERS.2014 = get.all.transmission.multipliers.malawi(sex="female",year=2015) 
        rv$MALE.AGE.MULTIPLIERS.2014 = get.all.transmission.multipliers.malawi(sex="male",year=2015)
        
    } else if(location=="Nigeria"){
        rv$FEMALE.AGE.MULTIPLIERS.2003 = get.all.transmission.multipliers.nigeria(sex="female",year=2003) 
        rv$MALE.AGE.MULTIPLIERS.2003 = get.all.transmission.multipliers.nigeria(sex="male",year=2003)
        
        rv$FEMALE.AGE.MULTIPLIERS.2008 = get.all.transmission.multipliers.nigeria(sex="female",year=2008) 
        rv$MALE.AGE.MULTIPLIERS.2008 = get.all.transmission.multipliers.nigeria(sex="male",year=2008)
        
        rv$FEMALE.AGE.MULTIPLIERS.2014 = get.all.transmission.multipliers.nigeria(sex="female",year=2013) 
        rv$MALE.AGE.MULTIPLIERS.2014 = get.all.transmission.multipliers.nigeria(sex="male",year=2013)
        
    # } else if(location=="Thailand"){
    #     rv$FEMALE.AGE.MULTIPLIERS.2003 = get.all.transmission.multipliers.thailand(sex="female",year=2006) 
    #     rv$MALE.AGE.MULTIPLIERS.2003 = get.all.transmission.multipliers.thailand(sex="male",year=2006)
    #     
    #     rv$FEMALE.AGE.MULTIPLIERS.2008 = get.all.transmission.multipliers.thailand(sex="female",year=2012) 
    #     rv$MALE.AGE.MULTIPLIERS.2008 = get.all.transmission.multipliers.thailand(sex="male",year=2012)
    #     
    #     rv$FEMALE.AGE.MULTIPLIERS.2014 = get.all.transmission.multipliers.thailand(sex="female",year=2014) 
    #     rv$MALE.AGE.MULTIPLIERS.2014 = get.all.transmission.multipliers.thailand(sex="male",year=2014)
    #     
    # } 
    }else if(location=="non.unaids.remainder"){
        rv$FEMALE.AGE.MULTIPLIERS.2003 = get.all.transmission.multipliers.india(sex="female",year=2005) 
        rv$MALE.AGE.MULTIPLIERS.2003 = get.all.transmission.multipliers.india(sex="male",year=2005)
        
        rv$FEMALE.AGE.MULTIPLIERS.2008 = get.all.transmission.multipliers.india(sex="female",year=2015) 
        rv$MALE.AGE.MULTIPLIERS.2008 = get.all.transmission.multipliers.india(sex="male",year=2015)
        
        rv$FEMALE.AGE.MULTIPLIERS.2014 = get.all.transmission.multipliers.india(sex="female",year=2019) 
        rv$MALE.AGE.MULTIPLIERS.2014 = get.all.transmission.multipliers.india(sex="male",year=2019)
        
    } else if(location=="unaids.remainder"){
        rv$FEMALE.AGE.MULTIPLIERS.2003 = get.all.transmission.multipliers.unaids(sex="female",year=2003) 
        rv$MALE.AGE.MULTIPLIERS.2003 = get.all.transmission.multipliers.unaids(sex="male",year=2003)
        
        rv$FEMALE.AGE.MULTIPLIERS.2008 = get.all.transmission.multipliers.unaids(sex="female",year=2008) 
        rv$MALE.AGE.MULTIPLIERS.2008 = get.all.transmission.multipliers.unaids(sex="male",year=2008)
        
        rv$FEMALE.AGE.MULTIPLIERS.2014 = get.all.transmission.multipliers.unaids(sex="female",year=2014) 
        rv$MALE.AGE.MULTIPLIERS.2014 = get.all.transmission.multipliers.unaids(sex="male",year=2014)
        
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
