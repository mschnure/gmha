
# Teeraananchai, 2025: Universal Health Coverage (UHC) programme in Thailand, 2014-2022
get.unsuppression.rate.thailand = function(){

    # Teeraananchai, 2025: Universal Health Coverage (UHC) programme in Thailand, 2014-2022
    # rate of 3.11 per 100 py 
    rate = 3.11/100 
    time = 1
    prob = 1 - exp(-rate*time)
    
    time.to.rebound = 1/rate # 32.15434 years - very long, but oh well 
    
    if(1==2){
        # Teeraananchai, 2022: TREAT Asia HIV Observational Database (TAHOD), 2003-2021
        # rate of 2.15 per 100 py 
        rate = 2.15/100 
        time = 1
        prob = 1 - exp(-rate*time)
        
        time.to.rebound = 1/rate # 46.51163 years 
        
        # Charoenpong, 2025: Population registered in national AIDS program database in 8 provinces, 2014-2018 
        # VF % after different time periods: 
        # 1 year: 2.5%; 2 years: 3.5%; 3 years: 4.6%; 4 years: 5.7%; 5 years: 6.2%
        prob = c(.025,.035,.046,.057,.062)
        time = c(1,2,3,4,5)
        
        rate = (-log(1-prob))/time
        prob = 1 - exp(-rate*time)
        
        time.to.rebound = 1/rate # 39.49789 56.13692 63.70562 68.15588 78.11849 years!   
    }

    rate
    
}

# 2012: 
# Crowell et al, 2016: Cohort in the Thai Red Cross AIDS Research Centre in Bangkok
# Time to suppression from ART initiation 
# 2009 - 2015 (using midpoint, 2012)

# 2013: 
# Kroon et al, 2018: (Same cohort as above, but reported from 2009-2017, so using 2013 as the year)
# Median time to suppression 

# 2016: 
# Eamsakulrat et al, 2022 (data are from 2016)
# % virally suppressed at 12 months 
get.suppression.rate.thailand = function(){
    max.proportion = 0.95
    
    # Crowell et al (2012 estimate)
    {
        # 17/261 did not achieve one-log decline after 2 weeks (244/261 did)
        # 29/263 did not achieve a two-log decline after 4 weeks (234/263 did)
        
        # 8 vs 16 weeks for those who DID achieve one-log decline after 2 weeks (244) vs not (17);
        # 12 vs 24 weeks for those who DID achieve two-log decline after 4 weeks (234) vs not (29)
        time.to.suppress.1 = (8*7)*(244/261) + (16*7)*(17/261)
        time.to.suppress.2 = (12*7)*(234/263) + (24*7)*(29/263)
        
        time.to.suppress = time.to.suppress.2 # using the longer one; 93.26236 days 
        
        time = c(time.to.suppress)/365
        rate = 1/time
        annual.proportion.2012 = 1 - exp(-rate)
    }
   
     # Kroon et al (2013 estimate)
    {
        #The median time to sustained viral suppression following ART initiation was 12 (2–60) weeks 
        #and occurred in 386 of 410 (94%) of study participants.
        time.in.days = 12*7 
        prob = 386/410
        time = c(time.in.days)/365
        rate = (-log(1-prob))/time
        # prob = 1 - exp(-rate*time)
        annual.proportion.2013 = 1 - exp(-rate)
    }
    
    # Eamsakulrat et al (2016 estimate)
    {
        # Of 270 who initiated ART, ... 202 (74.8%) individuals had HIV viral load suppression 
        # (78.9% after removing transfers out)
        annual.proportion.2016 = (202/256)
    }
    
    # this will create a NEGATIVE slope unless I anchor at 0 in 1990 (98% -> 99% -> 75%)
    data = c(0,annual.proportion.2012,annual.proportion.2013,annual.proportion.2016)
    
    dim.names.data = list(year = c(1990,2012,2013,2016),
                          age = c("10-19","20-29","30-39","40-49","50 and over"),
                          sex = c("male","female"))
    
    data.array = array(data,
                       dim = sapply(dim.names.data, length),
                       dimnames = dim.names.data)
    
    data.df = reshape2::melt(data.array)
    data.df$age = factor(data.df$age, levels = c("30-39","10-19","20-29","40-49","50 and over"))
    
    suppression.anchor.year = 1990
    suppression.years = as.numeric(dim.names.data$year)-suppression.anchor.year
    
    data.df$year = suppression.years
    
    fit = suppressWarnings(glm(value ~ year+age+sex, family=binomial, data = data.df)) 
    
    dim.names.projection = list(year = 1980:2040 - suppression.anchor.year,
                                age = c("10-19","20-29","30-39","40-49","50 and over"),
                                sex = c("male","female"))
    
    projection.array = array(NA,
                             dim = sapply(dim.names.projection,length),
                             dimnames = dim.names.projection)
    
    projection.array[as.character(dim.names.data$year - suppression.anchor.year),,] = data.array
    
    projection.df = reshape2::melt(projection.array)
    
    predictions = suppressWarnings(predict(fit,newdata=projection.df,type="response")) # predict off of model (same as multiplying coefficients)
    predictions = predictions*max.proportion
    dim(predictions) = sapply(dim.names.projection,length)
    dimnames(predictions) = dim.names.projection
    
    dimnames(predictions)$year = as.numeric(dimnames(predictions)$year) + suppression.anchor.year
    
    rv = list(intercept=fit$coefficients[1],
              slope = fit$coefficients[2], 
              age.10.19 = fit$coefficients[3],
              age.20.29 = fit$coefficients[4],
              age.40.49 = fit$coefficients[5],
              age.50.plus = fit$coefficients[6],
              sex.female = fit$coefficients[7],
              anchor.year=suppression.anchor.year,
              max.proportion=max.proportion,
              data.array = data.array,
              predictions=predictions)
    
    rv
    
}
