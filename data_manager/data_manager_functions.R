# Description: Functions used to read in and extract surveillance data for model calibration/inputs; 
# also used in plotting functions (analog to extract_data functions for simulation data) 

library(data.table)

# Higher-level core functions 
#     1. get.surveillance.data 
#     2. read.surveillance.data 
# Lower-level helper functions 
#     1. read.surveillance.data.type
#     2. read.surveillance.data.stratified 
#     3. read.surveillance.data.files 
#     4. read.cascade.data.type
#     5. read.cascade.data.stratified 
#     6. read.cascade.data.files 
# Specialty functions
#     1. read.pdf.data
#     2. read.population.data.files
#     3. read.fertility.data.files
#     4. read.death.data.files

# Population data from 2019 WPP archive: https://population.un.org/wpp/Download/Archive/CSV/ 
    #  NEW DEATHS DATA, 12/23/24: https://population.un.org/wpp/downloads    
        # WPP2024_MORT_F01_2_DEATHS_SINGLE_AGE_MALE.xlsx and female 
    # 2019 data
        # WPP2019_PopulationByAgeSex_Medium.csv
        # WPP2019_Fertility_by_Age.csv


# Each data.manager[[data.type]] is a list with the following elements:  
# $AGES
# $SEXES 
# $LOCATION
# $AGE.LOWERS (0-4 --> 0)
# $AGE.UPPERS (0-4 --> 5) - i.e., upper is exclusive
# $global, $age, $age.sex, etc. (this is the actual data - will vary by data type) 
# DON'T HAVE AGES/SEXES FOR BIRTHS 


# TO DO NOTES: 
# Add in what are the eligible values for each of the arguments below; as a comment
# Add in protections against bad inputs - warnings, etc. (although this should be robust; can pass anything)


##---------------------------------##
##-- HIGHER-LEVEL/CORE FUNCTIONS --##
##---------------------------------##

# Extracts data from data.manager object (created using call to read.surveillance.data function); called in 
# plotting functions but can be used on its own; allows user to specify which dimensions to look at and/or 
# stratify by (see simplot function) 
get.surveillance.data = function(data.manager,
                                 data.type,
                                 years = 2010:2015,
                                 ages = data.manager[[data.type]]$AGES, 
                                 sexes = data.manager[[data.type]]$SEXES, 
                                 locations = data.manager[[data.type]]$LOCATIONS,
                                 keep.dimensions = 'year'){
    if(!("year" %in% keep.dimensions))
        stop("Must keep dimension 'year'")
    
    if(!setequal(ages,data.manager[[data.type]]$AGES) & !("age" %in% keep.dimensions))
        stop("Ages are specified; must include ages in keep.dimensions")
    
    if(!setequal(sexes,data.manager[[data.type]]$SEXES) & !("sex" %in% keep.dimensions))
        stop("Sexes are specified; must include sexes in keep.dimensions")
    
    if(!setequal(locations,data.manager[[data.type]]$LOCATIONS) & !("location" %in% keep.dimensions))
        stop("Locations are specified; must include locations in keep.dimensions")
    
    pull.years = TRUE
    pull.ages = any(keep.dimensions=='age') 
    pull.sexes = any(keep.dimensions=='sex') 
    pull.locations = any(keep.dimensions=='location') 
    
    pull.dimensions = c('year','age','sex','location')
    pull.dimensions = pull.dimensions[c(pull.years, pull.ages, pull.sexes, pull.locations)]
    
    dim.names = list(year=as.character(years),
                     age=ages,
                     sex=sexes,
                     location=locations
    )
    dim.names = dim.names[pull.dimensions]
    
    rv = array(as.numeric(NA), 
               dim = sapply(dim.names, length), 
               dimnames = dim.names)
    
    if(setequal(pull.dimensions, 'year'))
        data.element = 'year'
    if(setequal(pull.dimensions, c('year','age')))
        data.element = 'year.age'
    if(setequal(pull.dimensions, c('year','sex')))
        data.element = 'year.sex'
    if(setequal(pull.dimensions, c('year','location')))
        data.element = 'year.location'
    if(setequal(pull.dimensions, c('year','age','sex')))
        data.element = 'year.age.sex'
    if(setequal(pull.dimensions, c('year','age','location')))
        data.element = 'year.age.location'
    if(setequal(pull.dimensions, c('year','sex','location')))
        data.element = 'year.sex.location'
    if(setequal(pull.dimensions, c('year','age','sex','location'))) 
        data.element = 'year.age.sex.location'
    
    data = data.manager[[data.type]][[data.element]]
    
    if(!is.null(data)){
        
        years.to.get = intersect(as.character(years), dimnames(data)$year)
        
        if(length(pull.dimensions)==1){
            rv[years.to.get] = data[years.to.get]
        }
        
        else if(setequal(pull.dimensions, c('year','age'))){ 
            ages.to.get = intersect(ages, dimnames(data)$age)
            rv[years.to.get, ages.to.get] = data[years.to.get, ages.to.get]
        }
        
        else if (setequal(pull.dimensions, c('year','sex'))){
            sexes.to.get = intersect(sexes, dimnames(data)$sex)
            rv[years.to.get, sexes.to.get] = data[years.to.get, sexes.to.get]
        }
        
        else if (setequal(pull.dimensions, c('year','location'))){
            locations.to.get = intersect(locations, dimnames(data)$location)
            rv[years.to.get, locations.to.get] = data[years.to.get, locations.to.get]
        }
        
        else if (setequal(pull.dimensions, c('year','age','sex'))){
            ages.to.get = intersect(ages, dimnames(data)$age)
            sexes.to.get = intersect(sexes, dimnames(data)$sex)
            rv[years.to.get, ages.to.get, sexes.to.get] = data[years.to.get, ages.to.get, sexes.to.get]
        }
        
        else if (setequal(pull.dimensions, c('year','age','location'))){
            ages.to.get = intersect(ages, dimnames(data)$age)
            locations.to.get = intersect(locations, dimnames(data)$location)
            rv[years.to.get, ages.to.get, locations.to.get] = data[years.to.get, ages.to.get, locations.to.get]
        }
        
        else if (setequal(pull.dimensions, c('year','sex','location'))){
            sexes.to.get = intersect(sexes, dimnames(data)$sex)
            locations.to.get = intersect(locations, dimnames(data)$location)
            rv[years.to.get, sexes.to.get, locations.to.get] = data[years.to.get, sexes.to.get, locations.to.get]
        }
        
        else if (setequal(pull.dimensions, c('year','age','sex','location'))){
            ages.to.get = intersect(ages, dimnames(data)$age)
            sexes.to.get = intersect(sexes, dimnames(data)$sex)
            locations.to.get = intersect(locations, dimnames(data)$location)
            rv[years.to.get, ages.to.get, sexes.to.get, locations.to.get] = data[years.to.get, ages.to.get, sexes.to.get, locations.to.get]
        }
        
        else stop("incorrect dimensions")
        
    }
    
    dim(rv) = sapply(dim.names, length)
    dimnames(rv) = dim.names
    
    rv
}

# Creates data.manager object; called in source code; calls different lower-level functions for each data 
# type (based on input file type) 
read.surveillance.data = function(dir = 'data_manager/data'){
    print("removed India for now")
    rv = list(date.created = Sys.Date())
    
    ## MAIN POINT ESTIMATES (no upper/lower suffix) ##
    rv$incidence = read.surveillance.data.type(data.type = 'incidence', suffix = "")
    rv$incidence$AGES = c('0-14','10-19','15-24','15-49','15+','50 and over')
    rv$incidence$AGE.LOWERS = c(0,10,15,15,15,50)
    rv$incidence$AGE.UPPERS = c(15,20,25,50,Inf,Inf)
    rv$incidence$SEXES = c('male','female')
    rv$incidence$LOCATIONS = dimnames(rv$incidence$year.location)$location
    
    rv$prevalence = read.surveillance.data.type(data.type = 'prevalence', suffix = "")
    rv$prevalence$AGES = c('0-14','10-19','15-24','15-49','15+','50 and over')
    rv$prevalence$AGE.LOWERS = c(0,10,15,15,15,50)
    rv$prevalence$AGE.UPPERS = c(15,20,25,50,Inf,Inf)
    rv$prevalence$SEXES = c('male','female')
    rv$prevalence$LOCATIONS = dimnames(rv$prevalence$year.location)$location
    
    rv$hiv.mortality = read.surveillance.data.type(data.type = 'hiv.mortality', suffix = "")
    rv$hiv.mortality$AGES = c('0-14','10-19','15-24','15-49','15+','50 and over')
    rv$hiv.mortality$AGE.LOWERS = c(0,10,15,15,15,50)
    rv$hiv.mortality$AGE.UPPERS = c(15,20,25,50,Inf,Inf)
    rv$hiv.mortality$SEXES = c('male','female')
    rv$hiv.mortality$LOCATIONS = dimnames(rv$hiv.mortality$year.location)$location
    
    ## Awareness denominator = all PLHIV
    rv$awareness = read.cascade.data.type(sub.data.type = "status", denominator = "allPLHIV", suffix = "")
    rv$awareness$AGES = c('15+')
    rv$awareness$AGE.LOWERS = c(15)
    rv$awareness$AGE.UPPERS = c(Inf)
    rv$awareness$SEXES = c('male','female')
    rv$awareness$LOCATIONS = dimnames(rv$awareness$year.location)$location

    ## Default engagement denominator = all aware PLHIV (option for all PLHIV below)
    rv$engagement = read.cascade.data.type(sub.data.type = "ART", denominator = "aware", suffix = "")
    rv$engagement$AGES = c('15+')
    rv$engagement$AGE.LOWERS = c(15)
    rv$engagement$AGE.UPPERS = c(Inf)
    rv$engagement$SEXES = c('male','female')
    rv$engagement$LOCATIONS = dimnames(rv$engagement$year.location)$location
        
    rv$engagement.allPLHIV = read.cascade.data.type(sub.data.type = "ART", denominator = "allPLHIV", suffix = "")
    rv$engagement.allPLHIV$AGES = c('15+')
    rv$engagement.allPLHIV$AGE.LOWERS = c(15)
    rv$engagement.allPLHIV$AGE.UPPERS = c(Inf)
    rv$engagement.allPLHIV$SEXES = c('male','female')
    rv$engagement.allPLHIV$LOCATIONS = dimnames(rv$engagement.allPLHIV$year.location)$location
    
    ## Default suppression denominator = all aware PLHIV (option for all PLHIV below)
    rv$suppression = read.cascade.data.type(sub.data.type = "suppress", denominator = "aware", suffix = "")
    rv$suppression$global = (rv$suppression$global*rv$engagement$global)
    rv$suppression$location = (rv$suppression$location*rv$engagement$location)
    rv$suppression$year.age.sex = (rv$suppression$year.age.sex*rv$engagement$year.age.sex)
    rv$suppression$year.age.sex.location = (rv$suppression$year.age.sex.location*rv$engagement$year.age.sex.location)
    
    rv$suppression$AGES = c('15+')
    rv$suppression$AGE.LOWERS = c(15)
    rv$suppression$AGE.UPPERS = c(Inf)
    rv$suppression$SEXES = c('male','female')
    rv$suppression$LOCATIONS = dimnames(rv$suppression$year.location)$location
    
    rv$suppression.allPLHIV = read.cascade.data.type(sub.data.type = "suppress", denominator = "allPLHIV", suffix = "")
    rv$suppression.allPLHIV$AGES = c('15+')
    rv$suppression.allPLHIV$AGE.LOWERS = c(15)
    rv$suppression.allPLHIV$AGE.UPPERS = c(Inf)
    rv$suppression.allPLHIV$SEXES = c('male','female')
    rv$suppression.allPLHIV$LOCATIONS = dimnames(rv$suppression.allPLHIV$year.location)$location
    
    
    
    
    ## UPPER/LOWER ESTIMATES ##
    rv$incidence.lowers = read.surveillance.data.type(data.type = 'incidence', suffix = "_lower")
    rv$incidence.uppers = read.surveillance.data.type(data.type = 'incidence', suffix = "_upper")
    rv$incidence.lowers$AGES = rv$incidence.uppers$AGES = c('0-14','10-19','15-24','15-49','15+','50 and over')
    rv$incidence.lowers$AGE.LOWERS = rv$incidence.uppers$AGE.LOWERS = c(0,10,15,15,15,50)
    rv$incidence.lowers$AGE.UPPERS = rv$incidence.uppers$AGE.UPPERS = c(15,20,25,50,Inf,Inf)
    rv$incidence.lowers$SEXES = rv$incidence.uppers$SEXES = c('male','female')
    rv$incidence.lowers$LOCATIONS = rv$incidence.uppers$LOCATIONS = dimnames(rv$incidence.lowers$year.location)$location

    rv$prevalence.lowers = read.surveillance.data.type(data.type = 'prevalence', suffix = "_lower")
    rv$prevalence.uppers = read.surveillance.data.type(data.type = 'prevalence', suffix = "_upper")
    rv$prevalence.lowers$AGES = rv$prevalence.uppers$AGES = c('0-14','10-19','15-24','15-49','15+','50 and over')
    rv$prevalence.lowers$AGE.LOWERS = rv$prevalence.uppers$AGE.LOWERS = c(0,10,15,15,15,50)
    rv$prevalence.lowers$AGE.UPPERS = rv$prevalence.uppers$AGE.UPPERS = c(15,20,25,50,Inf,Inf)
    rv$prevalence.lowers$SEXES = rv$prevalence.uppers$SEXES = c('male','female')
    rv$prevalence.lowers$LOCATIONS = rv$prevalence.uppers$LOCATIONS = dimnames(rv$prevalence.lowers$year.location)$location
    
    rv$hiv.mortality.lowers = read.surveillance.data.type(data.type = 'hiv.mortality', suffix = "_lower")
    rv$hiv.mortality.uppers = read.surveillance.data.type(data.type = 'hiv.mortality', suffix = "_upper")
    rv$hiv.mortality.lowers$AGES = rv$hiv.mortality.uppers$AGES = c('0-14','10-19','15-24','15-49','15+','50 and over')
    rv$hiv.mortality.lowers$AGE.LOWERS = rv$hiv.mortality.uppers$AGE.LOWERS = c(0,10,15,15,15,50)
    rv$hiv.mortality.lowers$AGE.UPPERS = rv$hiv.mortality.uppers$AGE.UPPERS = c(15,20,25,50,Inf,Inf)
    rv$hiv.mortality.lowers$SEXES = rv$hiv.mortality.uppers$SEXES = c('male','female')
    rv$hiv.mortality.lowers$LOCATIONS = rv$hiv.mortality.uppers$LOCATIONS = dimnames(rv$hiv.mortality.lowers$year.location)$location
    
    rv$awareness.lowers = read.cascade.data.type(sub.data.type = "status", denominator = "allPLHIV", suffix = "_lower")
    rv$awareness.uppers = read.cascade.data.type(sub.data.type = "status", denominator = "allPLHIV", suffix = "_upper")
    rv$awareness.lowers$AGES = rv$awareness.uppers$AGES = c('15+')
    rv$awareness.lowers$AGE.LOWERS = rv$awareness.uppers$AGE.LOWERS = c(15)
    rv$awareness.lowers$AGE.UPPERS = rv$awareness.uppers$AGE.UPPERS = c(Inf)
    rv$awareness.lowers$SEXES = rv$awareness.uppers$SEXES = c('male','female')
    rv$awareness.lowers$LOCATIONS = rv$awareness.uppers$LOCATIONS = dimnames(rv$awareness.lowers$year.location)$location
    
    ## Default engagement denominator = all aware PLHIV (option for all PLHIV below)
    rv$engagement.lowers = read.cascade.data.type(sub.data.type = "ART", denominator = "aware", suffix = "_lower")
    rv$engagement.uppers = read.cascade.data.type(sub.data.type = "ART", denominator = "aware", suffix = "_upper")
    rv$engagement.lowers$AGES = rv$engagement.uppers$AGES = c('15+')
    rv$engagement.lowers$AGE.LOWERS = rv$engagement.uppers$AGE.LOWERS = c(15)
    rv$engagement.lowers$AGE.UPPERS = rv$engagement.uppers$AGE.UPPERS = c(Inf)
    rv$engagement.lowers$SEXES = rv$engagement.uppers$SEXES = c('male','female')
    rv$engagement.lowers$LOCATIONS = rv$engagement.uppers$LOCATIONS = dimnames(rv$engagement.lowers$year.location)$location
    
    rv$engagement.allPLHIV.lowers = read.cascade.data.type(sub.data.type = "ART", denominator = "allPLHIV", suffix = "_lower")
    rv$engagement.allPLHIV.uppers = read.cascade.data.type(sub.data.type = "ART", denominator = "allPLHIV", suffix = "_upper")
    rv$engagement.allPLHIV.lowers$AGES = rv$engagement.allPLHIV.uppers$AGES = c('15+')
    rv$engagement.allPLHIV.lowers$AGE.LOWERS = rv$engagement.allPLHIV.uppers$AGE.LOWERS = c(15)
    rv$engagement.allPLHIV.lowers$AGE.UPPERS = rv$engagement.allPLHIV.uppers$AGE.UPPERS = c(Inf)
    rv$engagement.allPLHIV.lowers$SEXES = rv$engagement.allPLHIV.uppers$SEXES = c('male','female')
    rv$engagement.allPLHIV.lowers$LOCATIONS = rv$engagement.allPLHIV.uppers$LOCATIONS = 
        dimnames(rv$engagement.allPLHIV.lowers$year.location)$location
    
    ## Default suppression denominator = all aware PLHIV (option for all PLHIV below)
    rv$suppression.lowers = read.cascade.data.type(sub.data.type = "suppress", denominator = "aware", suffix = "_lower")
    rv$suppression.lowers$global = (rv$suppression.lowers$global*rv$engagement.lowers$global)
    rv$suppression.lowers$location = (rv$suppression.lowers$location*rv$engagement.lowers$location)
    rv$suppression.lowers$year.age.sex = (rv$suppression.lowers$year.age.sex*rv$engagement.lowers$year.age.sex)
    rv$suppression.lowers$year.age.sex.location = (rv$suppression.lowers$year.age.sex.location*rv$engagement.lowers$year.age.sex.location)
    rv$suppression.uppers = read.cascade.data.type(sub.data.type = "suppress", denominator = "aware", suffix = "_upper")
    rv$suppression.uppers$global = (rv$suppression.uppers$global*rv$engagement.uppers$global)
    rv$suppression.uppers$location = (rv$suppression.uppers$location*rv$engagement.uppers$location)
    rv$suppression.uppers$year.age.sex = (rv$suppression.uppers$year.age.sex*rv$engagement.uppers$year.age.sex)
    rv$suppression.uppers$year.age.sex.location = (rv$suppression.uppers$year.age.sex.location*rv$engagement.uppers$year.age.sex.location)
    
    rv$suppression.lowers$AGES = rv$suppression.uppers$AGES = c('15+')
    rv$suppression.lowers$AGE.LOWERS = rv$suppression.uppers$AGE.LOWERS = c(15)
    rv$suppression.lowers$AGE.UPPERS = rv$suppression.uppers$AGE.UPPERS = c(Inf)
    rv$suppression.lowers$SEXES = rv$suppression.uppers$SEXES = c('male','female')
    rv$suppression.lowers$LOCATIONS = rv$suppression.uppers$LOCATIONS = dimnames(rv$suppression.lowers$year.location)$location
    
    rv$suppression.allPLHIV.lowers = read.cascade.data.type(sub.data.type = "suppress", denominator = "allPLHIV", suffix = "_lower")
    rv$suppression.allPLHIV.uppers = read.cascade.data.type(sub.data.type = "suppress", denominator = "allPLHIV", suffix = "_upper")
    rv$suppression.allPLHIV.lowers$AGES = rv$suppression.allPLHIV.uppers$AGES = c('15+')
    rv$suppression.allPLHIV.lowers$AGE.LOWERS = rv$suppression.allPLHIV.uppers$AGE.LOWERS = c(15)
    rv$suppression.allPLHIV.lowers$AGE.UPPERS = rv$suppression.allPLHIV.uppers$AGE.UPPERS = c(Inf)
    rv$suppression.allPLHIV.lowers$SEXES = rv$suppression.allPLHIV.uppers$SEXES = c('male','female')
    rv$suppression.allPLHIV.lowers$LOCATIONS = rv$suppression.allPLHIV.uppers$LOCATIONS = 
        dimnames(rv$suppression.allPLHIV.lowers$year.location)$location
    
    
    

    # Population data aggregated into model age groups 
    rv$population = read.population.data.files.all.locations(data.type = "population",
                                                             countries.to.pull = COUNTRIES.TO.PULL.POP,
                                                             use.model.ages = T,
                                                             age.mapping = POPULATION.AGE.MAPPING.HARD.CODE)
    
    rv$population$AGES = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34",
                           "35-39","40-44","45-49","50-54","55-59","60-64","65-69",
                           "70-74","75-79","80 and over")
    rv$population$AGE.LOWERS = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80)
    rv$population$AGE.UPPERS = c(5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,Inf)
    rv$population$SEXES = c('male','female')
    rv$population$LOCATIONS = dimnames(rv$population$year.location)$location 
    
    # Full population data - population data kept in full age stratification 
    rv$population.full = read.population.data.files.all.locations(data.type = "population",
                                                                  countries.to.pull = COUNTRIES.TO.PULL.POP,
                                                                  use.model.ages = F,
                                                                  age.mapping = NULL)
    
   rv$population.full$AGES = c('0-4', '5-9','10-14','15-19','20-24','25-29','30-34',
                                '35-39','40-44','45-49','50-54','55-59','60-64','65-69',
                                '70-74','75-79','80-84','85-89','90-94','95-99','100 and over')
    rv$population.full$AGE.LOWERS = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100)
    rv$population.full$AGE.UPPERS = c(5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100,Inf)
    rv$population.full$SEXES = c('male','female')
    rv$population.full$LOCATIONS = dimnames(rv$population.full$year.location)$location 
    
    # Age-specific fertility rate 
    rv$fertility = read.fertility.data.files(data.type = "population",
                                             countries.to.pull = COUNTRIES.TO.PULL.POP)
    rv$fertility$YEARS = c("1953","1958","1963","1968","1973","1978","1983","1988","1993","1998","2003",
                           "2008","2013","2018","2023","2028","2033","2038","2043","2048","2053","2058",
                           "2063","2068","2073","2078","2083","2088","2093","2098")
    rv$fertility$AGES = c("15-19","20-24","25-29","30-34","35-39","40-44","45-49")
    rv$fertility$AGE.LOWERS = c(15,20,25,30,35,40,45)
    rv$fertility$AGE.UPPERS = c(20,25,30,35,40,45,50)
    rv$fertility$LOCATIONS = dimnames(rv$fertility$year.age.location)$location 

    #Deaths
    # rv$deaths.old = read.death.data.files.old(data.type = "population",
    #                                   countries.to.pull = COUNTRIES.TO.PULL.POP,
    #                                   age.mapping = DEATHS.AGE.MAPPING.HARD.CODE)
    # rv$deaths.old$YEARS = as.character(1950:2021)
    # rv$deaths.old$AGES = c('0-4', '5-9','10-14','15-19','20-24','25-29','30-34',
    #                    '35-39','40-44','45-49','50-54','55-59','60-64','65-69',
    #                    '70-74','75-79',"80 and over")
    # rv$deaths.old$AGE.LOWERS = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80)
    # rv$deaths.old$AGE.UPPERS = c(5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,Inf)
    # rv$deaths.old$SEXES = c('male','female')
    # rv$deaths.old$LOCATIONS = dimnames(rv$deaths.old$year.location)$location 
    
    # DEATHS FROM NEW DATA SOURCE
    rv$total.mortality = read.death.data.files(data.type = "population",
                                      countries.to.pull = COUNTRIES.TO.PULL.POP,
                                      age.mapping = DEATHS.AGE.MAPPING.HARD.CODE)
    rv$total.mortality$YEARS = as.character(1950:2023)
    rv$total.mortality$AGES = c('0-4', '5-9','10-14','15-19','20-24','25-29','30-34',
                       '35-39','40-44','45-49','50-54','55-59','60-64','65-69',
                       '70-74','75-79',"80 and over")
    rv$total.mortality$AGE.LOWERS = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80)
    rv$total.mortality$AGE.UPPERS = c(5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,Inf)
    rv$total.mortality$SEXES = c('male','female')
    rv$total.mortality$LOCATIONS = dimnames(rv$total.mortality$year.location)$location 
    
    
    rv
}


##----------------------------------##
##-- LOWER-LEVEL/HELPER FUNCTIONS --##
##----------------------------------##

# Called once for each data type within read.surveillance.data function; creates list with an array for 
# each data stratification (i.e., global, by age, by sex, by location, by age and location, etc.); calls 
# lower-level function, either read.surveillance.data.files (no stratification) or read.surveillance.data.stratified
read.surveillance.data.type = function(data.type,
                                       suffix){
    rv=list()
    
    rv$year = read.surveillance.data.files(data.type=data.type, 
                                            age='All ages',
                                            suffix = suffix)
    
    
    rv$year.location = read.surveillance.data.files(data.type=data.type,
                                               age='All ages',
                                               include.countries = T,
                                               suffix = suffix)
    
    ## Ages ##
    rv$year.age = read.surveillance.data.stratified(data.type=data.type,
                                               strata = 'age',
                                               include.countries = F,
                                               suffix = suffix)
    
    rv$year.age.location = read.surveillance.data.stratified(data.type=data.type,
                                                        strata = 'age',
                                                        include.countries = T,
                                                        suffix = suffix)
    
    ## Sexes ##
    rv$year.age.sex.location = read.surveillance.data.stratified(data.type=data.type,
                                                                 strata = 'year.age.sex',
                                                                 include.countries = T,
                                                                 suffix = suffix) 
    
    rv$sex.location = NULL

    rv
}

# Called once for each dimension (age/sex) within read.surveillance.data.type; loops through lower-level 
# function for each stratum of that dimension (i.e., calls function once for every age group) 
#     Age: read.surveillance.data.files
#     Sex: read.pdf.data
read.surveillance.data.stratified = function(data.type,
                                             strata,
                                             include.countries=T,
                                             suffix){
    ages=c('0-14','10-19','15-24','15-49','15+','50 and over','All ages')
    sexes = c("male","female")

    ## Pull array for age
    if(strata=='age') {
        ## Pull AGE array by COUNTRY
        if(include.countries) {
            age1 = read.surveillance.data.files(data.type=data.type,
                                                age=ages[1],
                                                include.countries = T,
                                                suffix = suffix)
            dim.names = c(dimnames(age1), list(age=ages))
            dim.names = dim.names[c(1,3,2)]
            
            rv = array(NA,
                       dim = sapply(dim.names, length),
                       dimnames = dim.names)
            
            for(i in 1:length(ages)){
                x = read.surveillance.data.files(data.type=data.type,
                                                 age=ages[i],
                                                 include.countries = T,
                                                 suffix = suffix)
                
                rv[,i,] = x 
            }
        } else {
        ## Pull TOTAL AGE array
            age1 = read.surveillance.data.files(data.type=data.type,
                                                age=ages[1],
                                                include.countries = F,
                                                suffix = suffix)
            dim.names = c(dimnames(age1), list(age=ages))
            rv = array(NA,
                       dim = sapply(dim.names, length),
                       dimnames = dim.names)
            
            for(i in 1:length(ages)){
                x = read.surveillance.data.files(data.type=data.type,
                                                 age=ages[i],
                                                 include.countries = F,
                                                 suffix = suffix)
                rv[,i] = x }
        }
    } else if(strata=='year.age.sex') {
        rv = read.pdf.data(data.type=data.type,
                           suffix=suffix)
        print("need age/sex pdfs for countries other than Kenya, South Africa, France, Mozambique, Tanzania")
    }
    else stop("only currently set up for age and year.age.sex strata")
    
    rv
    
    
} 

# Called once for each stratum of specified dimension within read.surveillance.data.stratified 
# Reads in csv files; formats data; returns an array of data with correct dimensions based on strata; 
# option to read in lower/upper bound files  
read.surveillance.data.files = function(dir = 'data_manager/data',
                                        data.type,
                                        include.countries = F,
                                        age,
                                        suffix){
   sub.dir = file.path(dir, data.type)
    # if(data.type=="incidence" & include.countries==T & age=="50 and over")
    #     browser()
    files = list.files(file.path(sub.dir))
    
    ## Total and locations
    age.to.match = gsub("\\+", "\\\\+", age)
    file = files[grepl(age.to.match,files)]
    
    if (length(file)!=1)
        stop("can only pull one file at a time")
    
    one.df = read.csv(file.path(sub.dir,file), row.names = 1)
    colnames(one.df) = substring(colnames(one.df),2)
    years = unique(substr(colnames(one.df),1,4))
    rownames(one.df)[rownames(one.df)=="United Republic of Tanzania"] = "Tanzania"
    location.names = rownames(one.df)[-nrow(one.df)]
    
    location.names = location.names[location.names!="India"] 
    if("India" %in% rownames(one.df))
        one.df = one.df[rownames(one.df)!="India",]
    
    one.df.t = transpose(one.df)
    rownames(one.df.t) <- colnames(one.df)
    colnames(one.df.t) <- rownames(one.df)
    
    # convert "<" values to median between values 
    suppressed.values = sapply(one.df.t,extract_suppressed_values)
    new.values = sapply(suppressed.values,pick_new_values)
    suppressed.values = lapply(suppressed.values,function(x){x[-1]})
    value.conversion = data.frame("suppressed.values"=unlist(suppressed.values),
                                  "new.values"=unlist(new.values))
    value.conversion$country = gsub("[0-9]+", "", rownames(value.conversion))
    
    row.names = rownames(one.df.t)
    one.df.t = data.frame(
        mapply(replace_with_conversion, one.df.t, names(one.df.t), MoreArgs = list(value.conversion = value.conversion), 
               SIMPLIFY = FALSE)
    )
    rownames(one.df.t) = row.names
    
    ## Need to put in check for suffix somehow
    ## Total ##
    dim.names.global = list(year=as.character(years)
    )
    
    global =  suppressWarnings(array(as.numeric(gsub(" ","",one.df.t[paste0(years, suffix),ncol(one.df.t)])),
                                    dim = sapply(dim.names.global, length), 
                                    dimnames = dim.names.global))
    
    
    ## Countries ##
    dim.names.locations = list(year=as.character(years),
                               location=location.names
    )
    
    
    locations =  suppressWarnings(array(as.integer(sapply(one.df.t[paste0(years, suffix),1:(length(location.names))], gsub, pattern = " ",replacement = "")),
                                        dim = sapply(dim.names.locations, length), 
                                        dimnames = dim.names.locations))
    
    
    
    if(include.countries==T)
        return(locations)
    
    else if(include.countries==F)
        return(global)
    
    
}

read.cascade.data.type = function(data.type="cascade",
                                  sub.data.type,
                                  denominator,
                                  suffix){
    rv=list()
    
    rv$year = read.cascade.data.files(data.type=data.type, 
                                       sub.data.type=sub.data.type,
                                       denominator=denominator,
                                       age='All ages',
                                       sex="All",
                                       suffix = suffix)
    
    
    rv$year.location = read.cascade.data.files(data.type=data.type,
                                          sub.data.type=sub.data.type,
                                          denominator=denominator,
                                          age='All ages',
                                          sex="All",
                                          include.countries = T,
                                          suffix = suffix)
    
    ## Year.Age.Sex ##
    rv$year.age.sex = read.cascade.data.stratified(data.type=data.type,
                                              sub.data.type=sub.data.type,
                                              denominator=denominator,
                                              include.countries = F,
                                              suffix = suffix)
    ## Year.Age.Sex.Country ##
    rv$year.age.sex.location = read.cascade.data.stratified(data.type=data.type,
                                                       sub.data.type=sub.data.type,
                                                       denominator=denominator,
                                                       include.countries = T,
                                                       suffix = suffix)
    
    rv
}

read.cascade.data.stratified = function(data.type,
                                        sub.data.type,
                                        denominator,
                                        include.countries=T,
                                        suffix){
    ## Pull AGE array by COUNTRY
    if(include.countries)
    {
        year.age.sex.location = read.cascade.data.files(data.type=data.type,
                                                   sub.data.type = sub.data.type,
                                                   denominator = denominator,
                                                   age="15+",
                                                   sex = "Female",
                                                   include.countries = T,
                                                   suffix = suffix)
        
        dim.names = c(dimnames(year.age.sex.location),list(age = "15+"),list(sex=c("male","female")))
        dim.names = dim.names[c(1,3,4,2)]
        
        rv = array(NA,
                   dim = sapply(dim.names, length),
                   dimnames = dim.names)
        
        rv[,,"female",] = year.age.sex.location
        rv[,,"male",] = read.cascade.data.files(data.type=data.type,
                                                sub.data.type = sub.data.type,
                                                denominator = denominator,
                                                age="15+",
                                                sex = "Male",
                                                include.countries = T,
                                                suffix = suffix)
    }
    ## Pull TOTAL AGE array
    else 
    {
        year.age.sex = read.cascade.data.files(data.type=data.type,
                                          sub.data.type = sub.data.type,
                                          denominator = denominator,
                                          age="15+",
                                          sex = "Male",
                                          include.countries = F,
                                          suffix = suffix)
        
        dim.names = c(dimnames(year.age.sex),list(age = "15+"),list(sex=c("male","female")))
        
        rv = array(NA,
                   dim = sapply(dim.names, length),
                   dimnames = dim.names)
        
        rv[,,"male"] = year.age.sex
        rv[,,"female"] = read.cascade.data.files(data.type=data.type,
                                                 sub.data.type = sub.data.type,
                                                 denominator = denominator,
                                                 age="15+",
                                                 sex = "Female",
                                                 include.countries = F,
                                                 suffix = suffix)  
    }
    
    rv
}

read.cascade.data.files = function(dir = 'data_manager/data',
                                   data.type,
                                   sub.data.type,
                                   denominator,
                                   include.countries = F,
                                   age,
                                   sex,
                                   suffix){
    sub.dir = file.path(dir, paste0(data.type,"_",denominator))
    
    files = list.files(file.path(sub.dir))
    
    ## Total and locations
    age.to.match = gsub("\\+", "\\\\+", age)
    file = files[grepl(age.to.match,files) & grepl(sex,files)]
    
    if (length(file)!=1)
        stop("can only pull one file at a time")
    
    one.df = read.csv(file.path(sub.dir,file), row.names = 1)
    colnames(one.df) = substring(colnames(one.df),2)
    years = unique(substr(colnames(one.df),1,4))
    rownames(one.df)[rownames(one.df)=="United Republic of Tanzania"] = "Tanzania"
    location.names = rownames(one.df)[c(-1,-nrow(one.df))]
    one.df = one.df[,!grepl("Footnote",names(one.df))]
    
    #sub.data.types = unique(trimws(one.df[1,]))
    one.df[1,] = rep(c(rep("status",3),rep("ART",3),rep("suppress",3)),length(years))
    one.df = one.df[,grepl(sub.data.type,one.df[1,])]
    one.df = one.df[-1,]
    if(sub.data.type!="status")
        colnames(one.df) = substring(colnames(one.df),1,nchar(colnames(one.df))-2)
    one.df.t = transpose(one.df)
    rownames(one.df.t) = colnames(one.df)
    colnames(one.df.t) = rownames(one.df)
    
    dim.names.global = list(year=as.character(years, suffix))

    global =  suppressWarnings(array((as.numeric(gsub(" ","",gsub(">","",one.df.t[paste0(years, suffix),ncol(one.df.t)])))/100),
                                    dim = sapply(dim.names.global, length), 
                                    dimnames = dim.names.global))
    
    
    ## Countries ##
    dim.names.locations = list(year=as.character(years),
                               location=location.names
    )
    
    
    locations =  suppressWarnings(array((as.integer(sapply(one.df.t[paste0(years, suffix),1:(length(location.names))],gsub, pattern = ">",replacement = ""))/100),
                                        dim = sapply(dim.names.locations, length), 
                                        dimnames = dim.names.locations))
     if(include.countries==T)
        return(locations)
    
    else if(include.countries==F)
        return(global)
    
}


##-------------------------##
##-- SPECIALTY FUNCTIONS --##
##-------------------------##
# Called for sex-specific incidence/prevalence/hiv mortality data in read.surveillance.data.stratified
read.pdf.data = function(dir = 'data_manager/data/pdfs',
                         data.type,
                         suffix){
    
    file = file.path(dir,"combined_pdfs.csv")
    if (length(file)!=1)
        stop("can only pull one file at a time")
    
    df = read.csv(file,header = T)
    rownames(df) = df[,1]
    years = substr(names(df),2,5)[-1]
    df = df[-1,-1]
    colnames(df) = years
    
    if(suffix=="")
        df = df[!grepl("upper",rownames(df)) & !grepl("lower",rownames(df)),]
    
    df = df[grepl(suffix,rownames(df)),]
    df = df[grepl(data.type,rownames(df)),]
    
    # put the data in the order of COUNTRIES.TO.PULL.PDFS
    locations_lower = convert_string(COUNTRIES.TO.PULL.PDFS)
    row_countries = gsub(paste0("_female_15_",data.type,suffix), "", rownames(df))
    row_countries = gsub(paste0("_male_15_",data.type,suffix), "", row_countries)
    custom_order = factor(row_countries, levels = locations_lower)
    df = df[order(custom_order, rownames(df)), ]
    
    ages = "15+"
    sexes = c("female","male") # will reverse this; just to pull in the correct order
    locations = COUNTRIES.TO.PULL.PDFS
    
    dim.names = list(sex = sexes,
                     location = locations,
                     year = years,
                     age = ages)

    rv = array(as.numeric(unname(unlist(df))),
               dim = sapply(dim.names,length),
               dimnames = dim.names)
    
    rv = aperm(rv,c(3,4,1,2))
    
    rv[,1,,] = rv[,,c(2,1),]
    dimnames(rv)$sex = c("male","female")
    
    rv
}

# Calls read.population.data.files for global then by location 
read.population.data.files.all.locations = function(dir = 'data_manager/data',
                                                    data.type,
                                                    countries.to.pull,
                                                    use.model.ages,
                                                    age.mapping){
    
    no.countries = read.population.data.files(data.type = data.type, 
                                              include.countries=F,
                                              countries.to.pull=NULL,
                                              use.model.ages = use.model.ages,
                                              age.mapping = age.mapping)
    
    countries = read.population.data.files(data.type = data.type, 
                                           include.countries=T,
                                           countries.to.pull=countries.to.pull,
                                           use.model.ages = use.model.ages,
                                           age.mapping = age.mapping)
    
    rv=list()
    
    # Global
    rv$year = no.countries$year
    
    ## Age ## 
    rv$year.age = no.countries$year.age
    
    ## Sex ##
    rv$year.sex = no.countries$year.sex
    ## Year.Age.Sex ##
    rv$year.age.sex = no.countries$year.age.sex
    
    # Location 
    rv$year.location = countries$year
    
    ## Age.Location ## 
    rv$year.age.location = countries$year.age
    
    ## Sex.Location ##
    rv$year.sex.location = countries$year.sex
    
    ## Year.Age.Sex.Location ##
    rv$year.age.sex.location = countries$year.age.sex
    
    rv
}

# Reads in population data files and returns data either in the age brackets as reported or in model age brackets; returns list with 
# an array for each stratification (total, age, sex, age*sex) 
read.population.data.files = function(dir = 'data_manager/data',
                                               data.type,
                                               include.countries = F,
                                               countries.to.pull,
                                               use.model.ages,
                                               age.mapping){
    sub.dir = file.path(dir, data.type)
    
    files = list.files(file.path(sub.dir))
    
    pop.file = "PopulationByAgeSex"
    file = files[grepl(pop.file,files)]
    
    if (length(file)!=1)
        stop("can only pull one file at a time")
    
    df = read.csv(file.path(sub.dir,file))
    years = unique(df$Time)
    data.ages = unique(df$AgeGrp)
    data.ages = c(data.ages[-length(data.ages)],"100 and over")
    
    if(use.model.ages){
        ages = names(age.mapping)
    } else{
        ages = data.ages
    }
        
    if(include.countries){
        # Set up arrays before filling them in over for loop for countries 

        ## Age array
        age.dim.names = list(year = as.character(years),
                             location = countries.to.pull,
                             age = ages)
        
        age = array(0,
                    dim = sapply(age.dim.names, length), 
                    dimnames = age.dim.names)
        
        total.dim.names = list(year = as.character(years),
                               location = countries.to.pull)
        
        ## Total array 
        total = array(apply(age,1:2,sum),
                      dim = sapply(total.dim.names, length), 
                      dimnames = total.dim.names)
        
        ## Age.Sex array
        sexes = c("male","female")

        age.sex.dim.names = list(year = as.character(years),
                                 location = countries.to.pull,
                                 age = ages,
                                 sex = sexes)
        
        male.age = array(0,
                         dim = sapply(age.dim.names, length), 
                         dimnames = age.dim.names)
        
        female.age = array(0,
                           dim = sapply(age.dim.names, length), 
                           dimnames = age.dim.names)
        
        sex.dim.names = list(year = as.character(years),
                             location = countries.to.pull,
                             sex = sexes)
        
        sex = array(0,
                    dim = sapply(sex.dim.names, length), 
                    dimnames = sex.dim.names)
        
        age.sex = array(0,
                        dim = sapply(age.sex.dim.names, length), 
                        dimnames = age.sex.dim.names)
        
        for(country in countries.to.pull){
            if(country=="Tanzania"){
                one.df = df[df$Location=="United Republic of Tanzania",]
            } else {
                one.df = df[df$Location==country,]
            }
                
            one.df$AgeGrp = factor(one.df$AgeGrp, levels = data.ages)
            one.df.sorted = one.df[order(one.df$AgeGrp),]
            
            age.one.country.dim.names = list(year = as.character(years),
                                             age = data.ages)
            
            age.one.country.full = array(as.numeric(one.df.sorted[,"PopTotal"])*1000,
                                         dim = sapply(age.one.country.dim.names,length),
                                         dimnames = age.one.country.dim.names)
            
            if(use.model.ages){
                age.one.country = sapply(1:length(age.mapping), function(a){
                    sapply(1:length(years), function(y){
                        age.to = names(age.mapping)[a] 
                        ages.from = age.mapping[[a]] 
                        sum(age.one.country.full[y,ages.from])
                    })
                })
                
                dimnames(age.one.country) = list(year = as.character(years),
                                                 age = names(age.mapping))
            } else {
                age.one.country = age.one.country.full
            }
            
            age[,country,] = age.one.country
            
            total[,country] = apply(age[,country,],"year",sum)
            
            male.age.one.country.full = array(as.numeric(one.df.sorted[,"PopMale"])*1000,
                                              dim = sapply(age.one.country.dim.names,length),
                                              dimnames = age.one.country.dim.names)
            female.age.one.country.full = array(as.numeric(one.df.sorted[,"PopFemale"])*1000,
                                                dim = sapply(age.one.country.dim.names,length),
                                                dimnames = age.one.country.dim.names)
            
            if(use.model.ages){
                male.age.one.country = sapply(1:length(age.mapping), function(a){
                    sapply(1:length(years), function(y){
                        age.to = names(age.mapping)[a] 
                        ages.from = age.mapping[[a]] 
                        sum(male.age.one.country.full[y,ages.from])
                    })
                })
                
                female.age.one.country = sapply(1:length(age.mapping), function(a){
                    sapply(1:length(years), function(y){
                        age.to = names(age.mapping)[a] 
                        ages.from = age.mapping[[a]] 
                        sum(female.age.one.country.full[y,ages.from])
                    })
                })
                
                dimnames(male.age.one.country) = dimnames(female.age.one.country) = 
                    list(year = as.character(years),
                         age = names(age.mapping))
            } else {
                male.age.one.country = male.age.one.country.full
                female.age.one.country = female.age.one.country.full
            }
            
            age.sex[,country,,"male"] = male.age.one.country 
            age.sex[,country,,"female"] = female.age.one.country
            
            ## Sex array
            male = array(apply(male.age.one.country,c("year"),sum),
                         dimnames = list(year = as.character(years)))
            
            female = array(apply(female.age.one.country,c("year"),sum),
                           dimnames = list(year = as.character(years)))
            
            sex[,country,"male"] = male
            sex[,country,"female"] = female
        }
        
        rv = list()
        rv$year = total
        rv$year.age = aperm(age,c(1,3,2))
        rv$year.sex = aperm(sex,c(1,3,2))
        rv$year.age.sex = aperm(age.sex,c(1,3,4,2))
        
    } else{
        df = df[df$Location=="World",]

        df$AgeGrp = factor(df$AgeGrp, levels = data.ages)
        df.sorted = df[order(df$AgeGrp),]
        
        ## Age array
        age.dim.names = list(year = as.character(years),
                             age = data.ages)
        
        age.full = array(0,
                    dim = sapply(age.dim.names, length), 
                    dimnames = age.dim.names)
        age.full[] = as.numeric(df.sorted[,"PopTotal"])*1000
        
        if(use.model.ages){
            # MAP TO MODEL AGES
            age = sapply(1:length(age.mapping), function(a){
                sapply(1:length(years), function(y){
                    age.to = names(age.mapping)[a] # names of mapping are the model ages - what I want to map TO
                    ages.from = age.mapping[[a]] # list elements are the population ages - what I want to map FROM
                    sum(age.full[y,ages.from])
                })
            })
            
            dimnames(age) = list(year = as.character(years),
                                 age = names(age.mapping))
        } else{
            age = age.full
        }
        
        ## Total array
        total = array(rowSums(age),
                       dimnames = list(year = as.character(years)))
        
        ## Age.Sex array
        sexes = c("male","female")

        male.age.full = array(0,
                         dim = sapply(age.dim.names, length), 
                         dimnames = age.dim.names)
        
        male.age.full[] = as.numeric(df.sorted[,"PopMale"])*1000
        
        female.age.full = array(0,
                           dim = sapply(age.dim.names, length), 
                           dimnames = age.dim.names)
        
        female.age.full[] = as.numeric(df.sorted[,"PopFemale"])*1000
        
        if(use.model.ages){
            # MAP TO MODEL AGES
            male.age = sapply(1:length(age.mapping), function(a){
                sapply(1:length(years), function(y){
                    age.to = names(age.mapping)[a] 
                    ages.from = age.mapping[[a]] 
                    sum(male.age.full[y,ages.from])
                })
            })
            
            female.age = sapply(1:length(age.mapping), function(a){
                sapply(1:length(years), function(y){
                    age.to = names(age.mapping)[a] 
                    ages.from = age.mapping[[a]] 
                    sum(female.age.full[y,ages.from])
                })
            })
            
            dimnames(male.age) = dimnames(female.age) = list(year = as.character(years),
                                                             age = names(age.mapping))
        } else{
            male.age = male.age.full
            female.age = female.age.full
        }
        
        age.sex.dim.names = list(year = as.character(years),
                                 age = dimnames(age)[[2]],
                                 sex = sexes)
        
        age.sex = array(0,
                        dim = sapply(age.sex.dim.names, length), 
                        dimnames = age.sex.dim.names)
        age.sex[,,"male"] = male.age
        age.sex[,,"female"] = female.age
        
        ## Sex array
        male = array(rowSums(male.age),
                     dimnames = list(year = as.character(years)))
        
        female = array(rowSums(female.age),
                       dimnames = list(year = as.character(years)))
        
        sex.dim.names = list(year = as.character(years),
                             sex = sexes)
        
        sex = array(0,
                    dim = sapply(sex.dim.names, length), 
                    dimnames = sex.dim.names)
        
        sex[,"male"] = male
        sex[,"female"] = female
        
        ## Returns a list
        rv = list()
        rv$year = total
        rv$year.age = age
        rv$year.sex = sex
        rv$year.age.sex = age.sex
        
    }
    rv
}

# Reads in age-specific fertility rate
read.fertility.data.files = function(dir = 'data_manager/data',
                                     data.type,
                                     countries.to.pull){
    sub.dir = file.path(dir, data.type)
    
    files = list.files(file.path(sub.dir))
    
    pop.file = "Fertility"
    file = files[grepl(pop.file,files)]
    
    if (length(file)!=1)
        stop("can only pull one file at a time")
    
    df = read.csv(file.path(sub.dir,file))
    df = df[df$Variant=="Medium",]
    
    countries.to.pull = c(countries.to.pull,"World")
    countries.to.pull[grepl("Tanzania",countries.to.pull)] = "United Republic of Tanzania"
    
    df = df[df$Location %in% countries.to.pull,]
    df$Location[grepl("Tanzania",df$Location)] = "Tanzania"
    #df = df[df$Location=="Kenya",]
    years = unique(df$MidPeriod)
    ages = unique(df$AgeGrp)
    locations = unique(df$Location)
    
    # reverse dim order so that array fills correctly (will transpose later)
    dim.names = list(age = ages,
                     year = as.character(years),
                     location = locations)
    
    fertility.rate = array(df$ASFR/1000,
                           dim = sapply(dim.names, length),
                           dimnames = dim.names)
    
    fertility.rate = aperm(fertility.rate,c(2,1,3)) # change dimension order 
    
    rv = list()
    rv$year.age.location = fertility.rate
    rv$year.age = fertility.rate[,,"World"]
    
    rv
}

# Reads in age/sex-specific death data 
read.death.data.files.old = function(dir = 'data_manager/data',
                                 data.type,
                                 countries.to.pull,
                                 age.mapping){
    sub.dir = file.path(dir, data.type)
    
    files = list.files(file.path(sub.dir))
    
    pop.file = "DeathsBySingleAgeSex"
    file = files[grepl(pop.file,files)]
    
    if (length(file)!=1)
        stop("can only pull one file at a time")
    
    df = read.csv(file.path(sub.dir,file))
    df = df[,c("Location","Time","AgeGrp","DeathMale","DeathFemale","DeathTotal")]
    countries.to.pull = c(countries.to.pull,"World")

    countries.to.pull[grepl("Tanzania",countries.to.pull)] = "United Republic of Tanzania"
    df = df[df$Location %in% countries.to.pull,]
    df$Location[grepl("Tanzania",df$Location)] = "Tanzania"
    
    years = unique(df$Time)
    ages.full = unique(df$AgeGrp)
    locations = unique(df$Location)
    sexes = c("male","female")
    
    dim.names = list(year = as.character(years),
                     age = names(age.mapping),
                     sex = sexes,
                     location = locations)
    
    year.age.full.dim.names = list(age = ages.full,
                                   year = as.character(years))
    
    year.age.sex.location = array(0,
                                  dim = sapply(dim.names, length),
                                  dimnames = dim.names)
    
    for(location in locations){
        female.age.full = male.age.full = array(0,
                                                dim = sapply(year.age.full.dim.names, length), 
                                                dimnames = year.age.full.dim.names)
        
        male.age.full[] = (df[df$Location==location,"DeathMale"])*1000
        female.age.full[] = (df[df$Location==location,"DeathFemale"])*1000
        
        male.age.full = aperm(male.age.full)
        female.age.full = aperm(female.age.full)
        
        male.age = sapply(1:length(age.mapping), function(a){
            sapply(1:length(years), function(y){
                age.to = names(age.mapping)[a] 
                ages.from = age.mapping[[a]] 
                sum(male.age.full[y,ages.from])
            })
        })
        
        female.age = sapply(1:length(age.mapping), function(a){
            sapply(1:length(years), function(y){
                age.to = names(age.mapping)[a] 
                ages.from = age.mapping[[a]] 
                sum(female.age.full[y,ages.from])
            })
        })
        
        dimnames(male.age) = dimnames(female.age) = list(year = as.character(years),
                                                         age = names(age.mapping))
        
        year.age.sex.location[,,"female",location] = female.age
        year.age.sex.location[,,"male",location] = male.age
    }

    
    year.sex.location = apply(year.age.sex.location,c("year","sex","location"),sum)
    year.age.location = apply(year.age.sex.location,c("year","age","location"),sum)
    year.location = apply(year.age.sex.location,c("year","location"),sum)
    
    year.age = apply(year.age.sex.location[,,,"World"],c("year","age"),sum)
    year.sex = apply(year.age.sex.location[,,,"World"],c("year","sex"),sum)
    year.age.sex = apply(year.age.sex.location[,,,"World"],c("year","age","sex"),sum)
    
    global = apply(year.age.sex.location[,,,"World"],c("year"),sum)
    
    ## Returns a list
    rv = list()
    rv$year = global
    rv$year.age = year.age
    rv$year.sex = year.sex
    rv$year.age.sex = year.age.sex
    rv$year.location = year.location
    rv$year.age.location = year.age.location
    rv$year.sex.location = year.sex.location
    rv$year.age.sex.location = year.age.sex.location
    
    rv
}

read.death.data.files = function(dir = 'data_manager/data',
                                 data.type,
                                 countries.to.pull,
                                 age.mapping){
    countries.to.pull = c(countries.to.pull,"World")
    countries.to.pull[grepl("Tanzania",countries.to.pull)] = "United Republic of Tanzania"
    
    sub.dir = file.path(dir, data.type)
    files = list.files(file.path(sub.dir))
    female.file = files[grepl("DEATHS_SINGLE_AGE_FEMALE",files)]
    male.file = files[grepl("DEATHS_SINGLE_AGE_MALE",files)]
    
    if (length(female.file)!=1)
        stop("can only pull one file at a time")
    
    if (length(male.file)!=1)
        stop("can only pull one file at a time")
    
    female.df = suppressWarnings(readxl::read_xlsx(file.path(sub.dir,female.file),skip = 16,col_names = T))
    female.df = female.df[,c(3,11:ncol(female.df))]
    colnames(female.df)[1] = "Location"
    female.df = female.df[female.df$Location %in% countries.to.pull,]
    female.df$Location[grepl("Tanzania",female.df$Location)] = "Tanzania"
    
    male.df = suppressWarnings(readxl::read_xlsx(file.path(sub.dir,male.file),skip = 16,col_names = T))
    male.df = male.df[,c(3,11:ncol(male.df))]
    colnames(male.df)[1] = "Location"
    male.df = male.df[male.df$Location %in% countries.to.pull,]
    male.df$Location[grepl("Tanzania",male.df$Location)] = "Tanzania"
    
    years = unique(female.df$Year)
    ages.full = colnames(female.df)[-c(1:2)]
    locations = unique(female.df$Location)
    sexes = c("male","female")
    
    dim.names = list(year = as.character(years),
                     age = names(age.mapping),
                     sex = sexes,
                     location = locations)
    
    year.age.full.dim.names = list(year = as.character(years),
                                   age = ages.full)
    
    year.age.sex.location = array(0,
                                  dim = sapply(dim.names, length),
                                  dimnames = dim.names)
    
    for(location in locations){
        female.age.full = male.age.full = array(0,
                                                dim = sapply(year.age.full.dim.names, length), 
                                                dimnames = year.age.full.dim.names)
        
        male.age.full[] = (as.numeric(unlist(male.df[male.df$Location==location,c(3:ncol(male.df))])))*1000
        female.age.full[] = (as.numeric(unlist(female.df[female.df$Location==location,c(3:ncol(female.df))])))*1000
        
        male.age = sapply(1:length(age.mapping), function(a){
            sapply(1:length(years), function(y){
                age.to = names(age.mapping)[a] 
                ages.from = age.mapping[[a]] 
                sum(male.age.full[y,ages.from])
            })
        })
        
        female.age = sapply(1:length(age.mapping), function(a){
            sapply(1:length(years), function(y){
                age.to = names(age.mapping)[a] 
                ages.from = age.mapping[[a]] 
                sum(female.age.full[y,ages.from])
            })
        })
        
        dimnames(male.age) = dimnames(female.age) = list(year = as.character(years),
                                                         age = names(age.mapping))
        
        year.age.sex.location[,,"female",location] = female.age
        year.age.sex.location[,,"male",location] = male.age
    }
    
    
    year.sex.location = apply(year.age.sex.location,c("year","sex","location"),sum)
    year.age.location = apply(year.age.sex.location,c("year","age","location"),sum)
    year.location = apply(year.age.sex.location,c("year","location"),sum)
    
    year.age = apply(year.age.sex.location[,,,"World"],c("year","age"),sum)
    year.sex = apply(year.age.sex.location[,,,"World"],c("year","sex"),sum)
    year.age.sex = apply(year.age.sex.location[,,,"World"],c("year","age","sex"),sum)
    
    global = apply(year.age.sex.location[,,,"World"],c("year"),sum)
    
    ## Returns a list
    rv = list()
    rv$year = global
    rv$year.age = year.age
    rv$year.sex = year.sex
    rv$year.age.sex = year.age.sex
    rv$year.location = year.location
    rv$year.age.location = year.age.location
    rv$year.sex.location = year.sex.location
    rv$year.age.sex.location = year.age.sex.location
    
    rv
}
