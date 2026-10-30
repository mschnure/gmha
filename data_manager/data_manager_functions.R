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

# Income status from World Bank: 
    # https://datahelpdesk.worldbank.org/knowledgebase/articles/906519-world-bank-country-and-lending-groups 
    # CLASS.xlsx


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
    if(data.type %in% c("suppression.oral","suppression.lai"))
        data.type="suppression"

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
    
    ## PLHIV by country/year: used to weight cascade proportions when combining countries into remainder models
    plhiv = rv$prevalence$year.location
    
    ## Awareness denominator = all PLHIV
    rv$awareness = read.cascade.data.type(sub.data.type = "status", denominator = "allPLHIV", suffix = "", plhiv = plhiv)
    rv$awareness$AGES = c('0-14','15+')
    rv$awareness$AGE.LOWERS = c(0,15)
    rv$awareness$AGE.UPPERS = c(15,Inf)
    rv$awareness$SEXES = c('male','female')
    rv$awareness$LOCATIONS = dimnames(rv$awareness$year.location)$location

    ## Default engagement denominator = all aware PLHIV (option for all PLHIV below)
    rv$engagement = read.cascade.data.type(sub.data.type = "ART", denominator = "aware", suffix = "", plhiv = plhiv)
    rv$engagement$AGES = c('0-14','15+')
    rv$engagement$AGE.LOWERS = c(0,15)
    rv$engagement$AGE.UPPERS = c(15,Inf)
    rv$engagement$SEXES = c('male','female')
    rv$engagement$LOCATIONS = dimnames(rv$engagement$year.location)$location
        
    rv$engagement.allPLHIV = read.cascade.data.type(sub.data.type = "ART", denominator = "allPLHIV", suffix = "", plhiv = plhiv)
    rv$engagement.allPLHIV$AGES = c('0-14','15+')
    rv$engagement.allPLHIV$AGE.LOWERS = c(0,15)
    rv$engagement.allPLHIV$AGE.UPPERS = c(15,Inf)
    rv$engagement.allPLHIV$SEXES = c('male','female')
    rv$engagement.allPLHIV$LOCATIONS = dimnames(rv$engagement.allPLHIV$year.location)$location
    
    ## Default suppression denominator = all aware PLHIV (option for all PLHIV below)
    rv$suppression = read.cascade.data.type(sub.data.type = "suppress", denominator = "aware", suffix = "", plhiv = plhiv)
        # multiply (suppress/on ART) * (on ART/aware) in order to make denominator aware: 
    rv$suppression$year = (rv$suppression$year*rv$engagement$year) # FIXED THIS STEP 10/3!! 
    rv$suppression$year.location = (rv$suppression$year.location*rv$engagement$year.location)
    rv$suppression$year.age = (rv$suppression$year.age*rv$engagement$year.age)
    rv$suppression$year.age.location = (rv$suppression$year.age.location*rv$engagement$year.age.location)
    rv$suppression$year.age.sex = (rv$suppression$year.age.sex*rv$engagement$year.age.sex)
    rv$suppression$year.age.sex.location = (rv$suppression$year.age.sex.location*rv$engagement$year.age.sex.location)
    
    rv$suppression$AGES = c('0-14','15+')
    rv$suppression$AGE.LOWERS = c(0,15)
    rv$suppression$AGE.UPPERS = c(15,Inf)
    rv$suppression$SEXES = c('male','female')
    rv$suppression$LOCATIONS = dimnames(rv$suppression$year.location)$location
    
    rv$suppression.allPLHIV = read.cascade.data.type(sub.data.type = "suppress", denominator = "allPLHIV", suffix = "", plhiv = plhiv)
    rv$suppression.allPLHIV$AGES = c('0-14','15+')
    rv$suppression.allPLHIV$AGE.LOWERS = c(0,15)
    rv$suppression.allPLHIV$AGE.UPPERS = c(15,Inf)
    rv$suppression.allPLHIV$SEXES = c('male','female')
    rv$suppression.allPLHIV$LOCATIONS = dimnames(rv$suppression.allPLHIV$year.location)$location
    
    # LAI vs oral suppression - just adding ages/sexes for plotting purposes
    rv$suppression.oral = NULL
    rv$suppression.oral$AGES = c('0-14','15+')
    rv$suppression.oral$AGE.LOWERS = c(0,15)
    rv$suppression.oral$AGE.UPPERS = c(15,Inf)
    rv$suppression.oral$SEXES = c('male','female')
    rv$suppression.oral$LOCATIONS = dimnames(rv$suppression$year.location)$location
    
    rv$suppression.lai = NULL
    rv$suppression.lai$AGES = c('0-14','15+')
    rv$suppression.lai$AGE.LOWERS = c(0,15)
    rv$suppression.lai$AGE.UPPERS = c(15,Inf)
    rv$suppression.lai$SEXES = c('male','female')
    rv$suppression.lai$LOCATIONS = dimnames(rv$suppression$year.location)$location
    
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
    
    rv$awareness.lowers = read.cascade.data.type(sub.data.type = "status", denominator = "allPLHIV", suffix = "_lower", plhiv = plhiv)
    rv$awareness.uppers = read.cascade.data.type(sub.data.type = "status", denominator = "allPLHIV", suffix = "_upper", plhiv = plhiv)
    rv$awareness.lowers$AGES = rv$awareness.uppers$AGES = c('0-14','15+')
    rv$awareness.lowers$AGE.LOWERS = rv$awareness.uppers$AGE.LOWERS = c(0,15)
    rv$awareness.lowers$AGE.UPPERS = rv$awareness.uppers$AGE.UPPERS = c(15,Inf)
    rv$awareness.lowers$SEXES = rv$awareness.uppers$SEXES = c('male','female')
    rv$awareness.lowers$LOCATIONS = rv$awareness.uppers$LOCATIONS = dimnames(rv$awareness.lowers$year.location)$location
    
    ## Default engagement denominator = all aware PLHIV (option for all PLHIV below)
    rv$engagement.lowers = read.cascade.data.type(sub.data.type = "ART", denominator = "aware", suffix = "_lower", plhiv = plhiv)
    rv$engagement.uppers = read.cascade.data.type(sub.data.type = "ART", denominator = "aware", suffix = "_upper", plhiv = plhiv)
    rv$engagement.lowers$AGES = rv$engagement.uppers$AGES = c('0-14','15+')
    rv$engagement.lowers$AGE.LOWERS = rv$engagement.uppers$AGE.LOWERS = c(0,15)
    rv$engagement.lowers$AGE.UPPERS = rv$engagement.uppers$AGE.UPPERS = c(15,Inf)
    rv$engagement.lowers$SEXES = rv$engagement.uppers$SEXES = c('male','female')
    rv$engagement.lowers$LOCATIONS = rv$engagement.uppers$LOCATIONS = dimnames(rv$engagement.lowers$year.location)$location
    
    rv$engagement.allPLHIV.lowers = read.cascade.data.type(sub.data.type = "ART", denominator = "allPLHIV", suffix = "_lower", plhiv = plhiv)
    rv$engagement.allPLHIV.uppers = read.cascade.data.type(sub.data.type = "ART", denominator = "allPLHIV", suffix = "_upper", plhiv = plhiv)
    rv$engagement.allPLHIV.lowers$AGES = rv$engagement.allPLHIV.uppers$AGES = c('0-14','15+')
    rv$engagement.allPLHIV.lowers$AGE.LOWERS = rv$engagement.allPLHIV.uppers$AGE.LOWERS = c(0,15)
    rv$engagement.allPLHIV.lowers$AGE.UPPERS = rv$engagement.allPLHIV.uppers$AGE.UPPERS = c(15,Inf)
    rv$engagement.allPLHIV.lowers$SEXES = rv$engagement.allPLHIV.uppers$SEXES = c('male','female')
    rv$engagement.allPLHIV.lowers$LOCATIONS = rv$engagement.allPLHIV.uppers$LOCATIONS = 
        dimnames(rv$engagement.allPLHIV.lowers$year.location)$location
    
    ## Default suppression denominator = all aware PLHIV (option for all PLHIV below)
    rv$suppression.lowers = read.cascade.data.type(sub.data.type = "suppress", denominator = "aware", suffix = "_lower", plhiv = plhiv)
        # multiply (suppress/on ART) * (on ART/aware) in order to make denominator aware: 
    # lowers x engagement lowers for every stratification (10/15/26: year/year.location used to use point engagement)
    rv$suppression.lowers$year = (rv$suppression.lowers$year*rv$engagement.lowers$year) # FIXED THIS STEP 10/3!! 
    rv$suppression.lowers$year.location = (rv$suppression.lowers$year.location*rv$engagement.lowers$year.location)
    rv$suppression.lowers$year.age = (rv$suppression.lowers$year.age*rv$engagement.lowers$year.age) # nextgen 0-14 data; now uses engagement.lowers (was point engagement) to match master's 10/15/26 fix
    rv$suppression.lowers$year.age.location = (rv$suppression.lowers$year.age.location*rv$engagement.lowers$year.age.location)
    rv$suppression.lowers$year.age.sex = (rv$suppression.lowers$year.age.sex*rv$engagement.lowers$year.age.sex)
    rv$suppression.lowers$year.age.sex.location = (rv$suppression.lowers$year.age.sex.location*rv$engagement.lowers$year.age.sex.location)
    rv$suppression.uppers = read.cascade.data.type(sub.data.type = "suppress", denominator = "aware", suffix = "_upper", plhiv = plhiv)
        # multiply (suppress/on ART) * (on ART/aware) in order to make denominator aware: 
    # uppers x engagement uppers for every stratification (10/15/26: year/year.location used to use point engagement)
    rv$suppression.uppers$year = (rv$suppression.uppers$year*rv$engagement.uppers$year) # FIXED THIS STEP 10/3!! 
    rv$suppression.uppers$year.location = (rv$suppression.uppers$year.location*rv$engagement.uppers$year.location)
    rv$suppression.uppers$year.age = (rv$suppression.uppers$year.age*rv$engagement.uppers$year.age) # nextgen 0-14 data; now uses engagement.uppers (was point engagement) to match master's 10/15/26 fix
    rv$suppression.uppers$year.age.location = (rv$suppression.uppers$year.age.location*rv$engagement.uppers$year.age.location)
    rv$suppression.uppers$year.age.sex = (rv$suppression.uppers$year.age.sex*rv$engagement.uppers$year.age.sex)
    rv$suppression.uppers$year.age.sex.location = (rv$suppression.uppers$year.age.sex.location*rv$engagement.uppers$year.age.sex.location)
    
    rv$suppression.lowers$AGES = rv$suppression.uppers$AGES = c('0-14','15+')
    rv$suppression.lowers$AGE.LOWERS = rv$suppression.uppers$AGE.LOWERS = c(0,15)
    rv$suppression.lowers$AGE.UPPERS = rv$suppression.uppers$AGE.UPPERS = c(15,Inf)
    rv$suppression.lowers$SEXES = rv$suppression.uppers$SEXES = c('male','female')
    rv$suppression.lowers$LOCATIONS = rv$suppression.uppers$LOCATIONS = dimnames(rv$suppression.lowers$year.location)$location
    
    rv$suppression.allPLHIV.lowers = read.cascade.data.type(sub.data.type = "suppress", denominator = "allPLHIV", suffix = "_lower", plhiv = plhiv)
    rv$suppression.allPLHIV.uppers = read.cascade.data.type(sub.data.type = "suppress", denominator = "allPLHIV", suffix = "_upper", plhiv = plhiv)
    rv$suppression.allPLHIV.lowers$AGES = rv$suppression.allPLHIV.uppers$AGES = c('0-14','15+')
    rv$suppression.allPLHIV.lowers$AGE.LOWERS = rv$suppression.allPLHIV.uppers$AGE.LOWERS = c(0,15)
    rv$suppression.allPLHIV.lowers$AGE.UPPERS = rv$suppression.allPLHIV.uppers$AGE.UPPERS = c(15,Inf)
    rv$suppression.allPLHIV.lowers$SEXES = rv$suppression.allPLHIV.uppers$SEXES = c('male','female')
    rv$suppression.allPLHIV.lowers$LOCATIONS = rv$suppression.allPLHIV.uppers$LOCATIONS = 
        dimnames(rv$suppression.allPLHIV.lowers$year.location)$location
    
    
    

    # Population data aggregated into model age groups 
    rv$population = read.population.data.files.all.locations(data.type = "population",
                                                             countries.to.pull = INDIVIDUAL.COUNTRIES,
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
                                                                  countries.to.pull = INDIVIDUAL.COUNTRIES,
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
                                             countries.to.pull = INDIVIDUAL.COUNTRIES)
    rv$fertility$YEARS = c("1953","1958","1963","1968","1973","1978","1983","1988","1993","1998","2003",
                           "2008","2013","2018","2023","2028","2033","2038","2043","2048","2053","2058",
                           "2063","2068","2073","2078","2083","2088","2093","2098")
    rv$fertility$AGES = c("15-19","20-24","25-29","30-34","35-39","40-44","45-49")
    rv$fertility$AGE.LOWERS = c(15,20,25,30,35,40,45)
    rv$fertility$AGE.UPPERS = c(20,25,30,35,40,45,50)
    rv$fertility$LOCATIONS = dimnames(rv$fertility$year.age.location)$location 

    #Deaths
    rv$total.mortality = read.death.data.files(data.type = "population",
                                      countries.to.pull = INDIVIDUAL.COUNTRIES,
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
# lower-level function, either: 
    # read.surveillance.data.files (no stratification), or 
    # read.surveillance.data.stratified (calls read.surveillance.data.files for each stratification, or pdf files for age/sex)
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
    # 10/30/26: by-sex data now from the UNAIDS AIDSinfo by-sex files ("..._Population_ Female adults (15+).csv", etc.) for
    # every age group that has them (10-19, 15-24, 15-49, 15+, 50 and over, All ages; none for 0-14), individual
    # countries + Global only (remainder locations NA). Replaces the hand-entered 15+ by-sex values from the UNAIDS
    # data book PDFs (data_manager/combined_pdfs.csv, read.pdf.data). "All ages" by sex also goes in year.sex.location.
    rv$year.age.sex.location = read.surveillance.data.by.sex(data.type=data.type,
                                                             suffix = suffix,
                                                             template = rv$year.age.location) 
    rv$year.sex.location = rv$year.age.sex.location[,"All ages",,]

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
                                        suffix,
                                        sex = "all"){ # "all", "male" or "female" (10/30/26: by-sex AIDSinfo files)
   sub.dir = file.path(dir, data.type)
    # if(data.type=="incidence" & include.countries==T & age=="50 and over")
    #     browser()
    files = list.files(file.path(sub.dir))
    
    ## Total and locations
    age.to.match = gsub("\\+", "\\\\+", age)
    file = files[grepl(age.to.match,files) & sapply(files, unaids.file.sex)==sex]
    
    if(sex!="all" && length(file)==0)
        return(NULL) # no by-sex file for this age group (e.g., 0-14)
    if (length(file)!=1)
        stop(paste0("can only pull one file at a time (",data.type,", ",age,", ",sex,"): ",paste0(file, collapse = "; ")))
    
    one.df = read.csv(file.path(sub.dir,file), row.names = 1)
    colnames(one.df) = substring(colnames(one.df),2)
    years = unique(substr(colnames(one.df),1,4))
    rownames(one.df)[rownames(one.df)=="United Republic of Tanzania"] = "Tanzania"
    
    location.names = rownames(one.df) # adding back in "Global" as a location 
    #location.names = rownames(one.df)[-nrow(one.df)] 
    
    location.names = location.names[location.names!="India"] 
    if("India" %in% rownames(one.df))
        one.df = one.df[rownames(one.df)!="India",]
    
    one.df.t = transpose(one.df)
    rownames(one.df.t) <- colnames(one.df)
    colnames(one.df.t) <- rownames(one.df)
    
    # convert "<" values to median between values 
    if(EXTRACT.SUPPRESSED.VALUES){
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
    }

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
    if(sex!="all")
        return(locations) # by-sex files: countries (+ Global) only; remainders aren't built from these
    locations[,"United States"] = NA
    
    # if(data.type=="prevalence") browser()
    
    unaids.remainder.countries = rowSums(locations[,REMAINDER.COUNTRIES.UNAIDS],na.rm = T) # add up all the remainder countries 
    non.unaids.remainder = global - rowSums(locations[,colnames(locations)!="Global"], na.rm = T) # global - everything that's in unaids (except global)
    
    # pull out the low, lower, upper, and high income countries that are in the remainder 
    low.income = rowSums(locations[,LOCATIONS.INCOME$low.remainder],na.rm = T)
    lower.middle.income = rowSums(locations[,LOCATIONS.INCOME$lower.middle.remainder],na.rm = T)
    upper.middle.income = rowSums(locations[,LOCATIONS.INCOME$upper.middle.remainder],na.rm = T)
    high.income = rowSums(locations[,LOCATIONS.INCOME$high.remainder],na.rm = T)
    
    # same thing with all 
    all.low = rowSums(locations[,LOCATIONS.INCOME$low.all],na.rm = T)
    all.lower.middle = rowSums(locations[,LOCATIONS.INCOME$lower.middle.all],na.rm = T)
    all.upper.middle = rowSums(locations[,LOCATIONS.INCOME$upper.middle.all],na.rm = T)
    all.high = rowSums(locations[,LOCATIONS.INCOME$high.all],na.rm = T)
    
    dim.names.locations.with.remainder = list(year=as.character(years),
                                              location=c(location.names,"unaids.remainder","non.unaids.remainder",
                                                         "r1.low","r1.lower.middle","r1.upper.middle","r1.high",
                                                         "all.low","all.lower.middle","all.upper.middle","all.high")
    )

    locations = array(c(locations,unaids.remainder.countries,non.unaids.remainder,
                        low.income,lower.middle.income,upper.middle.income,high.income,
                        all.low,all.lower.middle,all.upper.middle,all.high),
                      dim = sapply(dim.names.locations.with.remainder,length),
                      dimnames = dim.names.locations.with.remainder)
    
    locations[locations==0]=NA # r1.high will return 0 values for hiv.mortality in modern era; need this to be NA instead 
    
    if(include.countries==T)
        return(locations)
    
    else if(include.countries==F)
        return(global)
    
    
}

# 10/30/26: which sex a UNAIDS AIDSinfo file is for, from the "_Population_ ..." part of its name
#   e.g. "..._Population_ Young women (15-24).csv" -> "female"; "..._Population_ Men aged 50 and over.csv" -> "male";
#   "..._Population_ All adults (15+).csv" or no "_Population_" part (e.g. Children (0-14)) -> "all"
unaids.file.sex = function(file){
    if(!grepl("_Population_", file))
        return("all")
    population = tolower(sub(".*_Population_", "", file))
    if(grepl("female|women", population)) "female"
    else if(grepl("male|men", population)) "male"
    else "all"
}

# 10/30/26: by-sex incidence/prevalence/hiv mortality from the AIDSinfo by-sex files; returns a year x age x sex x location
# array with the same years/ages/locations as template (the year.age.location array of totals). Filled for
# locations.to.fill (individual countries + Global); everything else (remainders, 0-14) is NA.
read.surveillance.data.by.sex = function(data.type,
                                         suffix,
                                         template,
                                         locations.to.fill = c(INDIVIDUAL.COUNTRIES, "Global")){
    dim.names = list(year = dimnames(template)$year,
                     age = dimnames(template)$age,
                     sex = c("male","female"),
                     location = dimnames(template)$location)
    rv = array(as.numeric(NA), dim = sapply(dim.names, length), dimnames = dim.names)
    for(age in dim.names$age){
        for(sex in dim.names$sex){
            x = read.surveillance.data.files(data.type = data.type, age = age, sex = sex,
                                             include.countries = T, suffix = suffix)
            if(is.null(x))
                next
            years = intersect(dim.names$year, dimnames(x)$year)
            locations = intersect(locations.to.fill, dimnames(x)$location)
            rv[years, age, sex, locations] = x[years, locations]
        }
    }
    rv
}

read.cascade.data.type = function(data.type="cascade",
                                  sub.data.type,
                                  denominator,
                                  suffix,
                                  plhiv){
    rv=list()
    
    rv$year = read.cascade.data.files(data.type=data.type, 
                                       sub.data.type=sub.data.type,
                                       denominator=denominator,
                                       age='All ages',
                                       sex="All",
                                       suffix = suffix,
                                      plhiv = plhiv)
    
    rv$year.location = read.cascade.data.files(data.type=data.type,
                                          sub.data.type=sub.data.type,
                                          denominator=denominator,
                                          age='All ages',
                                          sex="All",
                                          include.countries = T,
                                          suffix = suffix,
                                      plhiv = plhiv)
    
    ## Year.Age (0-14) ##
    year.age = read.cascade.data.files(data.type=data.type,
                                       sub.data.type=sub.data.type,
                                       denominator=denominator,
                                       age='0-14',
                                       sex="",
                                       include.countries = F,
                                       suffix = suffix,
                                       plhiv = plhiv)
    # add 15+ as an age, add back in the age dimension
    year.age = array(c(year.age,rv$year), 
                     dim = c(dim(year.age)[1],age=2),
                     dimnames = list(year = dimnames(year.age)[[1]],
                                     age = c("0-14","15+")))
    rv$year.age = year.age
    
    ## Year.Age.Country (0-14) ## Also adding 15+ into this as well 
    year.age.location = read.cascade.data.files(data.type=data.type,
                                                sub.data.type=sub.data.type,
                                                denominator=denominator,
                                                age='0-14',
                                                sex="",
                                                include.countries = T,
                                                suffix = suffix,
                                       plhiv = plhiv)

    # 1. Define combined dimensions
    all_years <- dimnames(year.age.location)[[1]]
    all_locs  <- union(dimnames(year.age.location)[[2]],
                       dimnames(rv$year.location)[[2]])
    
    # 2. Create NA-filled arrays for both, covering all locations
    year.age.location_full <- array(
        NA,
        dim = c(length(all_years), length(all_locs)),
        dimnames = list(year = all_years, location = all_locs)
    )
    
    rv_year.location_full <- array(
        NA,
        dim = c(length(all_years), length(all_locs)),
        dimnames = list(year = all_years, location = all_locs)
    )
    
    # 3. Fill in available values
    year.age.location_full[, dimnames(year.age.location)[[2]]] <- year.age.location
    rv_year.location_full[, dimnames(rv$year.location)[[2]]]   <- rv$year.location
    
    # 4. Combine along the new "age" dimension
    combined <- array(
        NA,
        dim = c(
            year = length(all_years),
            age = 2,
            location = length(all_locs)
        ),
        dimnames = list(
            year = all_years,
            age = c("0-14", "15+"),
            location = all_locs
        )
    )
    
    combined[, "0-14", ] <- year.age.location_full
    combined[, "15+", ]  <- rv_year.location_full

    rv$year.age.location = combined
    
    ## Year.Age.Sex ##
    rv$year.age.sex = read.cascade.data.stratified(data.type=data.type,
                                              sub.data.type=sub.data.type,
                                              denominator=denominator,
                                              include.countries = F,
                                              suffix = suffix,
                                      plhiv = plhiv)
    ## Year.Age.Sex.Country ##
    rv$year.age.sex.location = read.cascade.data.stratified(data.type=data.type,
                                                       sub.data.type=sub.data.type,
                                                       denominator=denominator,
                                                       include.countries = T,
                                                       suffix = suffix,
                                      plhiv = plhiv)
    
    rv
}

read.cascade.data.stratified = function(data.type,
                                        sub.data.type,
                                        denominator,
                                        include.countries=T,
                                        suffix,
                                        plhiv){
    ## Pull AGE array by COUNTRY
    if(include.countries)
    {
        year.age.sex.location = read.cascade.data.files(data.type=data.type,
                                                   sub.data.type = sub.data.type,
                                                   denominator = denominator,
                                                   age="15+",
                                                   sex = "Female",
                                                   include.countries = T,
                                                   suffix = suffix,
                                      plhiv = plhiv)
        
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
                                                suffix = suffix,
                                      plhiv = plhiv)
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
                                          suffix = suffix,
                                      plhiv = plhiv)
        
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
                                                 suffix = suffix,
                                      plhiv = plhiv)  
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
                                   suffix,
                                   plhiv){
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

    # (10/30/26: no longer dropping 2024 - the 2025 UNAIDS cascade files have complete 2024 and 2025 data; master had already removed this)
        
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
    
    ## Combining countries into remainder models (rewritten 10/15/26)
    # Each group's value = PLHIV-weighted mean over the countries in that group (previously an
    # unweighted mean, which counted e.g. Afghanistan as much as DRC). Weights = all-ages PLHIV (prevalence) in that
    # country/year. To keep the same countries in every year (so a group's value doesn't jump when a big country stops
    # reporting), each country's gaps are filled first: linear interpolation between reported years; before its first /
    # after its last report, its nearest reported value moved in proportion to the global value (capped at 1) - holding
    # it flat instead biased early years upward, since many countries only start reporting ~2017-2018. Countries that
    # never report are left out.
    # non.unaids.remainder is backed out from the global value: global = w.indiv*indiv + w.unaids*unaids + w.non*non,
    # with w = each group's share of global PLHIV (replaces the pre-calculated CASCADE.WEIGHTS)
    plhiv.years = intersect(as.character(years), dimnames(plhiv)$year)
    if(length(plhiv.years)!=length(years))
        print(paste0("No PLHIV weights for cascade years: ",paste0(setdiff(years,plhiv.years),collapse=", "),
                     " (",sub.data.type,", ",denominator,", ",age,", ",sex,") - remainder values NA for those years"))
    
    global.trend = as.numeric(global) # same years/order as rows of locations
    fill.country.series = function(x){ # x = one country's values over the cascade years
        observed = which(!is.na(x))
        if(length(observed)==0)
            return(x)
        if(length(observed)==1)
            rv = rep(x[observed], length(x))
        else
            rv = approx(observed, x[observed], xout = seq_along(x), rule = 2)$y # flat outside observed range (fallback)
        first = min(observed)
        last = max(observed)
        before = seq_len(first-1)
        after = seq_along(x)[seq_along(x) > last]
        trend.before = x[first]*global.trend[before]/global.trend[first]
        trend.after = x[last]*global.trend[after]/global.trend[last]
        rv[before] = ifelse(is.na(trend.before), rv[before], pmin(1, trend.before))
        rv[after] = ifelse(is.na(trend.after), rv[after], pmin(1, trend.after))
        rv
    }
    
    plhiv.weighted.mean = function(countries){
        countries = intersect(countries, colnames(locations))
        rv = rep(as.numeric(NA), length(years))
        names(rv) = years
        if(length(countries)==0 || length(plhiv.years)==0)
            return(rv)
        values = locations[, countries, drop=F]
        values[] = apply(values, 2, fill.country.series) # same countries every year
        values = values[plhiv.years, , drop=F]
        weights = plhiv[plhiv.years, countries, drop=F]
        weights[is.na(values)] = NA
        values[is.na(weights)] = NA
        total.weight = rowSums(weights, na.rm = T)
        rv[plhiv.years] = ifelse(total.weight>0, rowSums(values*weights, na.rm = T)/total.weight, NA)
        rv
    }
    
    plhiv.share = function(countries){ # share of global PLHIV in these countries
        rv = rep(as.numeric(NA), length(years))
        names(rv) = years
        rv[plhiv.years] = rowSums(plhiv[plhiv.years, intersect(countries, colnames(plhiv)), drop=F], na.rm = T)/
            plhiv[plhiv.years, "Global"]
        rv
    }
    
    if(length(setdiff(REMAINDER.COUNTRIES.UNAIDS,dimnames(locations)[[2]]))!=0){
        countries.to.remove = setdiff(REMAINDER.COUNTRIES.UNAIDS,dimnames(locations)[[2]])
        print(paste0("Not in cascade datatype ",sub.data.type," (denominator = ",denominator,
                     "): ",paste0(countries.to.remove,collapse = ", "),"; age group: ",age))
    }
    
    individual.countries = plhiv.weighted.mean(INDIVIDUAL.COUNTRIES)
    unaids.remainder.countries = plhiv.weighted.mean(REMAINDER.COUNTRIES.UNAIDS)
    r1.low = plhiv.weighted.mean(LOCATIONS.INCOME$low.remainder)
    r1.lower.middle = plhiv.weighted.mean(LOCATIONS.INCOME$lower.middle.remainder)
    r1.upper.middle = plhiv.weighted.mean(LOCATIONS.INCOME$upper.middle.remainder)
    r1.high = plhiv.weighted.mean(LOCATIONS.INCOME$high.remainder)
    
    all.low = plhiv.weighted.mean(LOCATIONS.INCOME$low.all)
    all.lower.middle = plhiv.weighted.mean(LOCATIONS.INCOME$lower.middle.all)
    all.upper.middle = plhiv.weighted.mean(LOCATIONS.INCOME$upper.middle.all)
    all.high = plhiv.weighted.mean(LOCATIONS.INCOME$high.all)
    
    indiv.weight = plhiv.share(INDIVIDUAL.COUNTRIES)
    unaids.weight = plhiv.share(REMAINDER.COUNTRIES.UNAIDS)
    non.unaids.weight = 1 - indiv.weight - unaids.weight
    
    non.unaids.remainder.countries = (global - ((individual.countries*indiv.weight) + 
                                                    (unaids.remainder.countries*unaids.weight)))/non.unaids.weight
    
    # CHECK: weighted mean of the three groups should reproduce global
    # (individual.countries*indiv.weight) + (unaids.remainder.countries*unaids.weight) + (non.unaids.remainder.countries*non.unaids.weight) - global
    
    dim.names.locations.with.remainder = list(year=as.character(years),
                                              location=c(location.names,"unaids.remainder","non.unaids.remainder","Global",
                                                         "r1.low","r1.lower.middle","r1.upper.middle","r1.high",
                                                         "all.low","all.lower.middle","all.upper.middle","all.high")
    )
    
    locations = array(c(locations,unaids.remainder.countries,non.unaids.remainder.countries,global,
                        r1.low,r1.lower.middle,r1.upper.middle,r1.high,
                        all.low,all.lower.middle,all.upper.middle,all.high),
                      dim = sapply(dim.names.locations.with.remainder,length),
                      dimnames = dim.names.locations.with.remainder)
    
     if(include.countries==T)
        return(locations)
    
    else if(include.countries==F)
        return(global)
    
}


# Returns the UNAIDS locations whose all-ages PLHIV estimate is missing in every year (e.g., China, Russia,
# United States). Their PLHIV only enter the model through non.unaids.remainder (global - reported), so their
# population and deaths need to be assigned there too (see save_data_manager.R)
get.countries.without.prevalence.data = function(dir = 'data_manager/data'){
    sub.dir = file.path(dir, "prevalence")
    files = list.files(sub.dir)
    file = files[grepl("All ages", files)]
    if (length(file)!=1)
        stop("can only pull one file at a time")
    
    one.df = read.csv(file.path(sub.dir,file), row.names = 1, colClasses = "character")
    point.cols = grepl("^X[0-9]{4}$", colnames(one.df)) # point estimates only (not _lower/_upper/_Footnote)
    values = gsub("[ <>]", "", as.matrix(one.df[,point.cols]))
    has.data = apply(values, 1, function(x){any(!is.na(suppressWarnings(as.numeric(x))))})
    
    rownames(one.df)[!has.data]
}


# Cleans up lower/upper bounds for count data (incidence, prevalence, hiv.mortality); called in save_data_manager.R
# and again at the end of revise_overlapping_age_groups.R (10/15/26):
#   1. non.unaids.remainder bounds were global bound - sum of country bounds, which isn't a bound (lower often above
#      upper); now non.unaids gets the same relative interval as Global in that year/age
#   2. any remaining lower > point or upper < point (from suppressed "<" values / remainder scaling) is clamped to the point
#   3. negative counts are set to NA (dropped from the likelihood) - e.g., non.unaids.remainder hiv.mortality 10-19/15-24,
#      where global - sum of countries goes negative because most country values are imputed "<" midpoints
fix.count.bounds = function(data.manager,
                            data.types = c("incidence","prevalence","hiv.mortality"),
                            elements = c("year.location","year.age.location","year.sex.location","year.age.sex.location")){
    for(data.type in data.types){
        for(element in elements){
            pt = data.manager[[data.type]][[element]]
            lo = data.manager[[paste0(data.type,".lowers")]][[element]]
            up = data.manager[[paste0(data.type,".uppers")]][[element]]
            if(is.null(pt) || is.null(lo) || is.null(up))
                next
            if(!identical(dimnames(pt), dimnames(lo)) || !identical(dimnames(pt), dimnames(up)))
                stop(paste0("point/lower/upper dimensions don't match for ",data.type,"$",element))
            
            if(element=="year.location"){
                lo[,"non.unaids.remainder"] = pt[,"non.unaids.remainder"]*lo[,"Global"]/pt[,"Global"]
                up[,"non.unaids.remainder"] = pt[,"non.unaids.remainder"]*up[,"Global"]/pt[,"Global"]
            } else if(element=="year.age.location"){
                lo[,,"non.unaids.remainder"] = pt[,,"non.unaids.remainder"]*lo[,,"Global"]/pt[,,"Global"]
                up[,,"non.unaids.remainder"] = pt[,,"non.unaids.remainder"]*up[,,"Global"]/pt[,,"Global"]
            } else if(!element %in% c("year.sex.location","year.age.sex.location")) # by-sex: countries only, nothing to rescale (10/30/26)
                stop("fix.count.bounds only set up for year.location, year.age.location, year.sex.location and year.age.sex.location")
            
            negative = !is.na(pt) & pt<0
            if(any(negative)){
                print(paste0("Setting ",sum(negative)," negative values to NA in ",data.type,"$",element))
                pt[negative] = lo[negative] = up[negative] = NA
                data.manager[[data.type]][[element]] = pt
            }
            
            data.manager[[paste0(data.type,".lowers")]][[element]] = pmin(lo, pt) # pmin/pmax keep dims of first argument
            data.manager[[paste0(data.type,".uppers")]][[element]] = pmax(up, pt)
        }
    }
    data.manager
}

##-------------------------##
##-- SPECIALTY FUNCTIONS --##
##-------------------------##
# NO LONGER USED (10/30/26) - by-sex data now come from the AIDSinfo by-sex files (read.surveillance.data.by.sex).
# Was called for sex-specific incidence/prevalence/hiv mortality data in read.surveillance.data.stratified
read.pdf.data = function(dir = 'data_manager',
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

# Calls read.population.data.files, then splits into global and by location 
read.population.data.files.all.locations = function(dir = 'data_manager/data',
                                                    data.type,
                                                    countries.to.pull,
                                                    use.model.ages,
                                                    age.mapping){
    
    full.array = read.population.data.files(data.type = data.type, 
                                              countries.to.pull=countries.to.pull,
                                              use.model.ages = use.model.ages,
                                              age.mapping = age.mapping)
    
    rv=list()

    # Global
    rv$year = full.array$year[,"World"]
    
    ## Age ## 
    rv$year.age = full.array$year.age[,,"World"]
    
    ## Sex ##
    rv$year.sex = full.array$year.sex[,,"World"]
    ## Year.Age.Sex ##
    rv$year.age.sex = full.array$year.age.sex[,,,"World"]
    
    # Location - PUTTING "GLOBAL" BACK IN AS A LOCATION
    rv$year.location = full.array$year
    dimnames(rv$year.location)[["location"]][dimnames(rv$year.location)[["location"]]=="World"]="Global"
    #rv$year.location = full.array$year[,!dimnames(full.array$year)[["location"]] %in% "World"]
    
    
    ## Age.Location ## 
    rv$year.age.location = full.array$year.age
    dimnames(rv$year.age.location)[["location"]][dimnames(rv$year.age.location)[["location"]]=="World"]="Global"
    #rv$year.age.location = full.array$year.age[,,!dimnames(full.array$year)[["location"]] %in% "World"]
    
    ## Sex.Location ##
    rv$year.sex.location = full.array$year.sex
    dimnames(rv$year.sex.location)[["location"]][dimnames(rv$year.sex.location)[["location"]]=="World"]="Global"
    #rv$year.sex.location = full.array$year.sex[,,!dimnames(full.array$year)[["location"]] %in% "World"]
    
    ## Year.Age.Sex.Location ##
    rv$year.age.sex.location = full.array$year.age.sex
    dimnames(rv$year.age.sex.location)[["location"]][dimnames(rv$year.age.sex.location)[["location"]]=="World"]="Global"
    #rv$year.age.sex.location = full.array$year.age.sex[,,,!dimnames(full.array$year)[["location"]] %in% "World"]
    
    rv
}

# Reads in population data files and returns data either in the age brackets as reported or in model age brackets; returns list with 
# an array for each stratification (total, age, sex, age*sex) 
read.population.data.files = function(dir = 'data_manager/data',
                                               data.type,
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
    
    countries.to.pull = c(countries.to.pull,REMAINDER.COUNTRIES.UNAIDS,"World")
    
    countries.to.pull = countries.to.pull[!countries.to.pull %in% "Saint Kitts and Nevis"]
    # Set up arrays before filling them in over for loop for countries 
    ## Age array
    age.dim.names = list(year = as.character(years),
                         location = countries.to.pull,
                         age = ages)
    
    age = array(0,
                dim = sapply(age.dim.names, length), 
                dimnames = age.dim.names)
    
    ## Total array 
    total.dim.names = list(year = as.character(years),
                           location = countries.to.pull)
    
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
    
    
    missing.countries = setdiff(countries.to.pull, unique(df$Location))
    #print(missing.countries)
    
    for(country in countries.to.pull){

        if(country=="Tanzania"){
            one.df = df[df$Location=="United Republic of Tanzania",]
        } else if (country=="Democratic People's Republic of Korea"){
            one.df = df[df$Location=="Dem. People's Republic of Korea",]
        } else if (country=="Türkiye"){
            one.df = df[df$Location=="Turkey",]
        } else if (country=="United States"){
            one.df = df[df$Location=="United States of America",]
        } else {
            if(!country %in% unique(df$Location)){
                browser()
                #stop(paste0(country," is not in the population file (maybe named differently?)"))
            }
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
    
    age.sex.dim.names.remainder = list(year = as.character(years),
                                       age = ages,
                                       sex = sexes,
                                       location = c(countries.to.pull,"unaids.remainder","non.unaids.remainder",
                                                    "r1.low","r1.lower.middle","r1.upper.middle","r1.high",
                                                    "all.low","all.lower.middle","all.upper.middle","all.high"))
    
    total.dim.names.remainder = age.sex.dim.names.remainder[c(1,4)] # year, location 
    
    age.dim.names.remainder = age.sex.dim.names.remainder[c(1,2,4)] # year, age, location
    
    sex.dim.names.remainder = age.sex.dim.names.remainder[c(1,3,4)] # year, age, location
    
    all.pulled.countries = dimnames(rv$year)[[2]][!dimnames(rv$year)[[2]] %in% "World"] # individual countries + remainder countries 
    remainder.without.st.kitts = REMAINDER.COUNTRIES.UNAIDS[!REMAINDER.COUNTRIES.UNAIDS %in% "Saint Kitts and Nevis"]
    
    r1.low = LOCATIONS.INCOME$low.remainder[LOCATIONS.INCOME$low.remainder %in% remainder.without.st.kitts]
    r1.lower.middle = LOCATIONS.INCOME$lower.middle.remainder[LOCATIONS.INCOME$lower.middle.remainder %in% remainder.without.st.kitts]
    r1.upper.middle = LOCATIONS.INCOME$upper.middle.remainder[LOCATIONS.INCOME$upper.middle.remainder %in% remainder.without.st.kitts]
    r1.high = LOCATIONS.INCOME$high.remainder[LOCATIONS.INCOME$high.remainder %in% remainder.without.st.kitts]
    
    unaids.remainder.year = rowSums(rv$year[,remainder.without.st.kitts])
    r1.low.year = rowSums(rv$year[,r1.low])
    r1.lower.middle.year = rowSums(rv$year[,r1.lower.middle])
    r1.upper.middle.year = rowSums(rv$year[,r1.upper.middle])
    r1.high.year = rowSums(rv$year[,r1.high])
    non.unaids.remainder.year = rv$year[,"World"] - rowSums(rv$year[,all.pulled.countries])
    
    unaids.remainder.year.age = apply(rv$year.age[,,remainder.without.st.kitts],c(1:2),sum)
    r1.low.year.age = apply(rv$year.age[,,r1.low],c(1:2),sum)
    r1.lower.middle.year.age = apply(rv$year.age[,,r1.lower.middle],c(1:2),sum)
    r1.upper.middle.year.age = apply(rv$year.age[,,r1.upper.middle],c(1:2),sum)
    r1.high.year.age = apply(rv$year.age[,,r1.high],c(1:2),sum)
    non.unaids.remainder.year.age = rv$year.age[,,"World"] - apply(rv$year.age[,,all.pulled.countries],c(1:2),sum)
    # check 
    # table(unaids.remainder.year.age - (r1.low.year.age + r1.lower.middle.year.age + r1.upper.middle.year.age + r1.high.year.age))
    
    unaids.remainder.year.sex = apply(rv$year.sex[,,remainder.without.st.kitts],c(1:2),sum)
    r1.low.year.sex = apply(rv$year.sex[,,r1.low],c(1:2),sum)
    r1.lower.middle.year.sex = apply(rv$year.sex[,,r1.lower.middle],c(1:2),sum)
    r1.upper.middle.year.sex = apply(rv$year.sex[,,r1.upper.middle],c(1:2),sum)
    r1.high.year.sex = apply(rv$year.sex[,,r1.high],c(1:2),sum)
    non.unaids.remainder.year.sex = rv$year.sex[,,"World"] - apply(rv$year.sex[,,all.pulled.countries],c(1:2),sum)
    
    unaids.remainder.year.age.sex = apply(rv$year.age.sex[,,,remainder.without.st.kitts],c(1:3),sum)
    r1.low.year.age.sex = apply(rv$year.age.sex[,,,r1.low],c(1:3),sum)
    r1.lower.middle.year.age.sex = apply(rv$year.age.sex[,,,r1.lower.middle],c(1:3),sum)
    r1.upper.middle.year.age.sex = apply(rv$year.age.sex[,,,r1.upper.middle],c(1:3),sum)
    r1.high.year.age.sex = apply(rv$year.age.sex[,,,r1.high],c(1:3),sum)
    non.unaids.remainder.year.age.sex = rv$year.age.sex[,,,"World"] - apply(rv$year.age.sex[,,,all.pulled.countries],c(1:3),sum)
    
    # x = unaids.remainder.year.age.sex + non.unaids.remainder.year.age.sex
    # z = apply(rv$year.age.sex[,,,COUNTRIES.TO.PULL.POP],c(1:3),sum)
    # y = rv$year.age.sex[,,,"World"]
    # table(y - (x+z)) # world - (remainder + individual)
    
    ## 4/7/26 - adding "all.low", etc.
    all.low = LOCATIONS.INCOME$low.all
    all.lower.middle = LOCATIONS.INCOME$lower.middle.all
    all.upper.middle = LOCATIONS.INCOME$upper.middle.all
    all.high = LOCATIONS.INCOME$high.all[LOCATIONS.INCOME$high.all %in% remainder.without.st.kitts]
    
    all.low.year = rowSums(rv$year[,all.low])
    all.lower.middle.year = rowSums(rv$year[,all.lower.middle])
    all.upper.middle.year = rowSums(rv$year[,all.upper.middle])
    all.high.year = rowSums(rv$year[,all.high])
    
    all.low.year.age = apply(rv$year.age[,,all.low],c(1:2),sum)
    all.lower.middle.year.age = apply(rv$year.age[,,all.lower.middle],c(1:2),sum)
    all.upper.middle.year.age = apply(rv$year.age[,,all.upper.middle],c(1:2),sum)
    all.high.year.age = apply(rv$year.age[,,all.high],c(1:2),sum)
        
    all.low.year.sex = apply(rv$year.sex[,,all.low],c(1:2),sum)
    all.lower.middle.year.sex = apply(rv$year.sex[,,all.lower.middle],c(1:2),sum)
    all.upper.middle.year.sex = apply(rv$year.sex[,,all.upper.middle],c(1:2),sum)
    all.high.year.sex = apply(rv$year.sex[,,all.high],c(1:2),sum)
    
    all.low.year.age.sex = apply(rv$year.age.sex[,,,all.low],c(1:3),sum)
    all.lower.middle.year.age.sex = apply(rv$year.age.sex[,,,all.lower.middle],c(1:3),sum)
    all.upper.middle.year.age.sex = apply(rv$year.age.sex[,,,all.upper.middle],c(1:3),sum)
    all.high.year.age.sex = apply(rv$year.age.sex[,,,all.high],c(1:3),sum)
    
    rv$year = array(c(rv$year,unaids.remainder.year,non.unaids.remainder.year,
                      r1.low.year,r1.lower.middle.year,r1.upper.middle.year,r1.high.year,
                      all.low.year,all.lower.middle.year,all.upper.middle.year,all.high.year),
                    dim = sapply(total.dim.names.remainder,length),
                    dimnames = total.dim.names.remainder)
    rv$year.age = array(c(rv$year.age,unaids.remainder.year.age,non.unaids.remainder.year.age,
                          r1.low.year.age,r1.lower.middle.year.age,r1.upper.middle.year.age,r1.high.year.age,
                          all.low.year.age,all.lower.middle.year.age,all.upper.middle.year.age,all.high.year.age),
                        dim = sapply(age.dim.names.remainder,length),
                        dimnames = age.dim.names.remainder)
    rv$year.sex = array(c(rv$year.sex,unaids.remainder.year.sex,non.unaids.remainder.year.sex,
                          r1.low.year.sex,r1.lower.middle.year.sex,r1.upper.middle.year.sex,r1.high.year.sex,
                          all.low.year.sex,all.lower.middle.year.sex,all.upper.middle.year.sex,all.high.year.sex),
                        dim = sapply(sex.dim.names.remainder,length),
                        dimnames = sex.dim.names.remainder)
    rv$year.age.sex = array(c(rv$year.age.sex,unaids.remainder.year.age.sex,non.unaids.remainder.year.age.sex,
                              r1.low.year.age.sex,r1.lower.middle.year.age.sex,r1.upper.middle.year.age.sex,r1.high.year.age.sex,
                              all.low.year.age.sex,all.lower.middle.year.age.sex,all.upper.middle.year.age.sex,all.high.year.age.sex),
                            dim = sapply(age.sex.dim.names.remainder,length),
                            dimnames = age.sex.dim.names.remainder)
    
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
    unaids.remainder=df[df$Location=="World",]
    unaids.remainder$Location="unaids.remainder"
    
    non.unaids.remainder=df[df$Location=="World",]
    non.unaids.remainder$Location="non.unaids.remainder"
    
    r1.low=df[df$Location=="World",]
    r1.lower.middle = r1.upper.middle = r1.high = r1.low
    r1.low$Location = "r1.low"
    r1.lower.middle$Location = "r1.lower.middle"
    r1.upper.middle$Location = "r1.upper.middle"
    r1.high$Location = "r1.high"
    
    df = rbind(df,unaids.remainder,non.unaids.remainder,
               r1.low,r1.lower.middle,r1.upper.middle,r1.high)
    
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
read.death.data.files = function(dir = 'data_manager/data',
                                 data.type,
                                 countries.to.pull,
                                 age.mapping){
    countries.to.pull = c("World",countries.to.pull,REMAINDER.COUNTRIES.UNAIDS)
    countries.to.pull[grepl("Tanzania",countries.to.pull)] = "United Republic of Tanzania"
    countries.to.pull[grepl("Democratic People's Republic of Korea",countries.to.pull)] = "Dem. People's Republic of Korea"
    countries.to.pull[grepl("United States",countries.to.pull)] = "United States of America"
    
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
    
    # missing.countries = setdiff(countries.to.pull, unique(female.df$Location))
    female.df = female.df[female.df$Location %in% countries.to.pull,]
    female.df$Location[grepl("Tanzania",female.df$Location)] = "Tanzania"
    female.df$Location[grepl("Dem. People's Republic of Korea",female.df$Location)] = "Democratic People's Republic of Korea"
    female.df$Location[grepl("United States of America",female.df$Location)] = "United States"
    female.df$Location[grepl("World",female.df$Location)] = "Global"
    
    male.df = suppressWarnings(readxl::read_xlsx(file.path(sub.dir,male.file),skip = 16,col_names = T))
    male.df = male.df[,c(3,11:ncol(male.df))]
    colnames(male.df)[1] = "Location"
    male.df = male.df[male.df$Location %in% countries.to.pull,]
    male.df$Location[grepl("Tanzania",male.df$Location)] = "Tanzania"
    male.df$Location[grepl("Dem. People's Republic of Korea",male.df$Location)] = "Democratic People's Republic of Korea"
    male.df$Location[grepl("United States of America",male.df$Location)] = "United States"
    male.df$Location[grepl("World",male.df$Location)] = "Global"
    
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
    
    year.age.location = apply(year.age.sex.location,c("year","age","location"),sum)
    year.sex.location = apply(year.age.sex.location,c("year","sex","location"),sum)
    year.location = apply(year.age.sex.location,c("year","location"),sum)
    
    year.unaids.remainder = apply(year.location[,REMAINDER.COUNTRIES.UNAIDS],c("year"),sum)
    year.age.unaids.remainder = apply(year.age.location[,,REMAINDER.COUNTRIES.UNAIDS],c("year","age"),sum)
    year.sex.unaids.remainder = apply(year.sex.location[,,REMAINDER.COUNTRIES.UNAIDS],c("year","sex"),sum)
    year.age.sex.unaids.remainder = apply(year.age.sex.location[,,,REMAINDER.COUNTRIES.UNAIDS],c("year","age","sex"),sum)
    
    
    
    # BY INCOME, REMAINDER - low
    year.r1.low = apply(year.location[,LOCATIONS.INCOME$low.remainder],c("year"),sum)
    year.age.r1.low = apply(year.age.location[,,LOCATIONS.INCOME$low.remainder],c("year","age"),sum)
    year.sex.r1.low = apply(year.sex.location[,,LOCATIONS.INCOME$low.remainder],c("year","sex"),sum)
    year.age.sex.r1.low = apply(year.age.sex.location[,,,LOCATIONS.INCOME$low.remainder],c("year","age","sex"),sum)
    
    # lower middle
    year.r1.lower.middle = apply(year.location[,LOCATIONS.INCOME$lower.middle.remainder],c("year"),sum)
    year.age.r1.lower.middle = apply(year.age.location[,,LOCATIONS.INCOME$lower.middle.remainder],c("year","age"),sum)
    year.sex.r1.lower.middle = apply(year.sex.location[,,LOCATIONS.INCOME$lower.middle.remainder],c("year","sex"),sum)
    year.age.sex.r1.lower.middle = apply(year.age.sex.location[,,,LOCATIONS.INCOME$lower.middle.remainder],c("year","age","sex"),sum)
    
    # upper middle 
    year.r1.upper.middle = apply(year.location[,LOCATIONS.INCOME$upper.middle.remainder],c("year"),sum)
    year.age.r1.upper.middle = apply(year.age.location[,,LOCATIONS.INCOME$upper.middle.remainder],c("year","age"),sum)
    year.sex.r1.upper.middle = apply(year.sex.location[,,LOCATIONS.INCOME$upper.middle.remainder],c("year","sex"),sum)
    year.age.sex.r1.upper.middle = apply(year.age.sex.location[,,,LOCATIONS.INCOME$upper.middle.remainder],c("year","age","sex"),sum)
    
    # high 
    year.r1.high = apply(year.location[,LOCATIONS.INCOME$high.remainder],c("year"),sum)
    year.age.r1.high = apply(year.age.location[,,LOCATIONS.INCOME$high.remainder],c("year","age"),sum)
    year.sex.r1.high = apply(year.sex.location[,,LOCATIONS.INCOME$high.remainder],c("year","sex"),sum)
    year.age.sex.r1.high = apply(year.age.sex.location[,,,LOCATIONS.INCOME$high.remainder],c("year","age","sex"),sum)
    
    
    # BY INCOME, ALL - added 6/2/26
    # low
    year.all.low = apply(year.location[,LOCATIONS.INCOME$low.all],c("year"),sum)
    year.age.all.low = apply(year.age.location[,,LOCATIONS.INCOME$low.all],c("year","age"),sum)
    year.sex.all.low = apply(year.sex.location[,,LOCATIONS.INCOME$low.all],c("year","sex"),sum)
    year.age.sex.all.low = apply(year.age.sex.location[,,,LOCATIONS.INCOME$low.all],c("year","age","sex"),sum)
    
    # lower middle
    year.all.lower.middle = apply(year.location[,LOCATIONS.INCOME$lower.middle.all],c("year"),sum)
    year.age.all.lower.middle = apply(year.age.location[,,LOCATIONS.INCOME$lower.middle.all],c("year","age"),sum)
    year.sex.all.lower.middle = apply(year.sex.location[,,LOCATIONS.INCOME$lower.middle.all],c("year","sex"),sum)
    year.age.sex.all.lower.middle = apply(year.age.sex.location[,,,LOCATIONS.INCOME$lower.middle.all],c("year","age","sex"),sum)
    
    # upper middle 
    year.all.upper.middle = apply(year.location[,LOCATIONS.INCOME$upper.middle.all],c("year"),sum)
    year.age.all.upper.middle = apply(year.age.location[,,LOCATIONS.INCOME$upper.middle.all],c("year","age"),sum)
    year.sex.all.upper.middle = apply(year.sex.location[,,LOCATIONS.INCOME$upper.middle.all],c("year","sex"),sum)
    year.age.sex.all.upper.middle = apply(year.age.sex.location[,,,LOCATIONS.INCOME$upper.middle.all],c("year","age","sex"),sum)
    
    # high 
    year.all.high = apply(year.location[,LOCATIONS.INCOME$high.all],c("year"),sum)
    year.age.all.high = apply(year.age.location[,,LOCATIONS.INCOME$high.all],c("year","age"),sum)
    year.sex.all.high = apply(year.sex.location[,,LOCATIONS.INCOME$high.all],c("year","sex"),sum)
    year.age.sex.all.high = apply(year.age.sex.location[,,,LOCATIONS.INCOME$high.all],c("year","age","sex"),sum)
    
    
    
    ## Global 
    year.age = apply(year.age.sex.location[,,,"Global"],c("year","age"),sum)
    year.sex = apply(year.age.sex.location[,,,"Global"],c("year","sex"),sum)
    year.age.sex = apply(year.age.sex.location[,,,"Global"],c("year","age","sex"),sum)
    
    global = apply(year.age.sex.location[,,,"Global"],c("year"),sum)
    
    all.pulled.countries = !(grepl("Global",dimnames(year.location)[[2]])) # individual and unaids remainder
    
    year.non.unaids.remainder = global - apply(year.location[,all.pulled.countries],c("year"),sum)
    year.age.non.unaids.remainder = year.age - apply(year.age.location[,,all.pulled.countries],c("year","age"),sum)
    year.sex.non.unaids.remainder = year.sex - apply(year.sex.location[,,all.pulled.countries],c("year","sex"),sum)
    year.age.sex.non.unaids.remainder = year.age.sex - apply(year.age.sex.location[,,,all.pulled.countries],c("year","age","sex"),sum)
    
    age.sex.dim.names.remainder = list(year = as.character(years),
                                       age = names(age.mapping),
                                       sex = sexes,
                                       location = c(locations,"unaids.remainder","non.unaids.remainder",
                                                    "r1.low","r1.lower.middle","r1.upper.middle","r1.high",
                                                    "all.low","all.lower.middle","all.upper.middle","all.high"))
    
    total.dim.names.remainder = age.sex.dim.names.remainder[c(1,4)] # year, location 
    
    age.dim.names.remainder = age.sex.dim.names.remainder[c(1,2,4)] # year, age, location
    
    sex.dim.names.remainder = age.sex.dim.names.remainder[c(1,3,4)] # year, age, location
    
    ## Returns a list
    rv = list()
    rv$year = global
    rv$year.age = year.age
    rv$year.sex = year.sex
    rv$year.age.sex = year.age.sex
    
    rv$year.location = array(c(year.location,year.unaids.remainder,year.non.unaids.remainder,
                               year.r1.low,year.r1.lower.middle,year.r1.upper.middle,year.r1.high,
                               year.all.low,year.all.lower.middle,year.all.upper.middle,year.all.high),
                             dim = sapply(total.dim.names.remainder,length),
                             dimnames = total.dim.names.remainder)
    rv$year.age.location =  array(c(year.age.location,year.age.unaids.remainder,year.age.non.unaids.remainder,
                                    year.age.r1.low,year.age.r1.lower.middle,year.age.r1.upper.middle,year.age.r1.high,
                                    year.age.all.low,year.age.all.lower.middle,year.age.all.upper.middle,year.age.all.high),
                                  dim = sapply(age.dim.names.remainder,length),
                                  dimnames = age.dim.names.remainder)
    
    rv$year.sex.location =  array(c(year.sex.location,year.sex.unaids.remainder,year.sex.non.unaids.remainder,
                                    year.sex.r1.low,year.sex.r1.lower.middle,year.sex.r1.upper.middle,year.sex.r1.high,
                                    year.sex.all.low,year.sex.all.lower.middle,year.sex.all.upper.middle,year.sex.all.high),
                                  dim = sapply(sex.dim.names.remainder,length),
                                  dimnames = sex.dim.names.remainder)
    rv$year.age.sex.location =  array(c(year.age.sex.location,year.age.sex.unaids.remainder,year.age.sex.non.unaids.remainder,
                                        year.age.sex.r1.low,year.age.sex.r1.lower.middle,year.age.sex.r1.upper.middle,year.age.sex.r1.high,
                                        year.age.sex.all.low,year.age.sex.all.lower.middle,year.age.sex.all.upper.middle,year.age.sex.all.high),
                                      dim = sapply(age.sex.dim.names.remainder,length),
                                      dimnames = age.sex.dim.names.remainder)
    
    rv
}


# read.death.data.files.old = function(dir = 'data_manager/data',
#                                      data.type,
#                                      countries.to.pull,
#                                      age.mapping){
#     sub.dir = file.path(dir, data.type)
#     
#     files = list.files(file.path(sub.dir))
#     
#     pop.file = "DeathsBySingleAgeSex"
#     file = files[grepl(pop.file,files)]
#     
#     if (length(file)!=1)
#         stop("can only pull one file at a time")
#     
#     df = read.csv(file.path(sub.dir,file))
#     df = df[,c("Location","Time","AgeGrp","DeathMale","DeathFemale","DeathTotal")]
#     countries.to.pull = c(countries.to.pull,"World")
#     
#     countries.to.pull[grepl("Tanzania",countries.to.pull)] = "United Republic of Tanzania"
#     df = df[df$Location %in% countries.to.pull,]
#     df$Location[grepl("Tanzania",df$Location)] = "Tanzania"
#     
#     years = unique(df$Time)
#     ages.full = unique(df$AgeGrp)
#     locations = unique(df$Location)
#     sexes = c("male","female")
#     
#     dim.names = list(year = as.character(years),
#                      age = names(age.mapping),
#                      sex = sexes,
#                      location = locations)
#     
#     year.age.full.dim.names = list(age = ages.full,
#                                    year = as.character(years))
#     
#     year.age.sex.location = array(0,
#                                   dim = sapply(dim.names, length),
#                                   dimnames = dim.names)
#     
#     for(location in locations){
#         female.age.full = male.age.full = array(0,
#                                                 dim = sapply(year.age.full.dim.names, length), 
#                                                 dimnames = year.age.full.dim.names)
#         
#         male.age.full[] = (df[df$Location==location,"DeathMale"])*1000
#         female.age.full[] = (df[df$Location==location,"DeathFemale"])*1000
#         
#         male.age.full = aperm(male.age.full)
#         female.age.full = aperm(female.age.full)
#         
#         male.age = sapply(1:length(age.mapping), function(a){
#             sapply(1:length(years), function(y){
#                 age.to = names(age.mapping)[a] 
#                 ages.from = age.mapping[[a]] 
#                 sum(male.age.full[y,ages.from])
#             })
#         })
#         
#         female.age = sapply(1:length(age.mapping), function(a){
#             sapply(1:length(years), function(y){
#                 age.to = names(age.mapping)[a] 
#                 ages.from = age.mapping[[a]] 
#                 sum(female.age.full[y,ages.from])
#             })
#         })
#         
#         dimnames(male.age) = dimnames(female.age) = list(year = as.character(years),
#                                                          age = names(age.mapping))
#         
#         year.age.sex.location[,,"female",location] = female.age
#         year.age.sex.location[,,"male",location] = male.age
#     }
#     
#     
#     year.sex.location = apply(year.age.sex.location,c("year","sex","location"),sum)
#     year.age.location = apply(year.age.sex.location,c("year","age","location"),sum)
#     year.location = apply(year.age.sex.location,c("year","location"),sum)
#     
#     year.age = apply(year.age.sex.location[,,,"World"],c("year","age"),sum)
#     year.sex = apply(year.age.sex.location[,,,"World"],c("year","sex"),sum)
#     year.age.sex = apply(year.age.sex.location[,,,"World"],c("year","age","sex"),sum)
#     
#     global = apply(year.age.sex.location[,,,"World"],c("year"),sum)
#     
#     ## Returns a list
#     rv = list()
#     rv$year = global
#     rv$year.age = year.age
#     rv$year.sex = year.sex
#     rv$year.age.sex = year.age.sex
#     rv$year.location = year.location
#     rv$year.age.location = year.age.location
#     rv$year.sex.location = year.sex.location
#     rv$year.age.sex.location = year.age.sex.location
#     
#     rv
# }