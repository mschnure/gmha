
# Eamsakulrat et al, 2022 (data are from 2016)
get.disengagement.model.thailand = function(){
    disengagement.p = (1-0.841) # 84.1% individuals retained in care at 12 months 
    
    disengagement.rate.unsuppressed = -log(1-disengagement.p)
    disengagement.rate.suppressed = disengagement.rate.unsuppressed
    
    rv = list()
    rv$disengagement.rate.unsuppressed = disengagement.rate.unsuppressed
    rv$disengagement.rate.suppressed = disengagement.rate.suppressed
    
    rv

}


# Eamsakulrat et al, 2022 (data are from 2016)
# Compared early vs late ART initiation groups (<= or > 2 weeks)
# Median time from HIV diagnosis to ART initiation  
get.engagement.model.thailand = function(){
    
    # I want total diagnosed/not on ART --> on ART 
    # 488 newly diagnosed - 98 transferred out - 20 LTFU - 24 didn't receive diagnosis - 47 transferred later = 299 enrolled in HIV clinic
    # removing those who didn't receive their diagnosis (shouldn't be in the denominator):
        # 488 - 24 = 464 received diagnosis 
    # removing those who we don't know whether they started ART (transferred out)" 
        # 464 - (98+47) = 319 
    # KEEP in those who were LTFU - we know they received their diagnosis but didn't start ART 
        # --> 319 total diagnosed in the denominator 
    
    # Of those, 270 started ART 
        # Early group (116): median time to initiation was 7 days 
        # Late group (154): median time to initiation was 28 days 
    
    ## original calculations led to 100% enrollment in a year, so instead, just using 270/299 enrolled within 1 year 
    time.in.days = 365 # 7*(116/270) + 28*(154/270) # 18.97778 days (weighted average time)
    prob = 270/299 # 270/319
    
    # rate = (-log(1-prob))/time
    # prob = 1 - exp(-rate*time)
    
    time = c(time.in.days)/365
    rate = (-log(1-prob))/time
    annual.proportion = 1 - exp(-rate)
    data = c(0,annual.proportion)
    
    dim.names.data = list(year = c(1990,2016),
                          age = c("10-19","20-29","30-39","40-49","50 and over"),
                          sex = c("male","female"))
    
    data.array = array(data,
                       dim = sapply(dim.names.data, length),
                       dimnames = dim.names.data)
    
    data.df = reshape2::melt(data.array)
    data.df$age = factor(data.df$age, levels = c("30-39","10-19","20-29","40-49","50 and over"))
    
    engagement.anchor.year = 1990
    engagement.years = as.numeric(dim.names.data$year)-engagement.anchor.year
    
    data.df$year = engagement.years
    
    fit = suppressWarnings(glm(value ~ year+age+sex, family=binomial, data = data.df)) 
    
    max.proportion = 0.95
    
    dim.names.projection = list(year = 1980:2040 - engagement.anchor.year,
                                age = c("10-19","20-29","30-39","40-49","50 and over"),
                                sex = c("male","female"))
    
    projection.array = array(NA,
                             dim = sapply(dim.names.projection,length),
                             dimnames = dim.names.projection)
    
    projection.array[as.character(dim.names.data$year - engagement.anchor.year),,] = data.array
    
    projection.df = reshape2::melt(projection.array)
    
    predictions = suppressWarnings(predict(fit,newdata=projection.df,type="response")) # predict off of model (same as multiplying coefficients)
    predictions = predictions*max.proportion
    dim(predictions) = sapply(dim.names.projection,length)
    dimnames(predictions) = dim.names.projection
    
    dimnames(predictions)$year = as.numeric(dimnames(predictions)$year) + engagement.anchor.year
    
    rv = list(intercept=fit$coefficients[1],
              slope = fit$coefficients[2], 
              age.10.19 = fit$coefficients[3],
              age.20.29 = fit$coefficients[4],
              age.40.49 = fit$coefficients[5],
              age.50.plus = fit$coefficients[6],
              sex.female = fit$coefficients[7],
              anchor.year=engagement.anchor.year,
              max.proportion=max.proportion,
              data.array = data.array,
              predictions=predictions)
    
    rv
    
}


