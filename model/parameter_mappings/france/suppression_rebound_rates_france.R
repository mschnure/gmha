


get.unsuppression.rate.france = function(){
    
    ## Cuzin et al, 2023
    # Among those followed, 91% still had a controlled VL 1 year later
    prob = c(1-0.91)
    time = c(1)
    # --> rate = 0.09431068 --> time to rebound = 10.60325 years 
    
    rate = (-log(1-prob))/time
    prob = 1 - exp(-rate*time)
    
    time.to.rebound = 1/rate
    
    rate
    
}


# Cuzin, 2023: Dat'AIDS cohort, 2009-2019
# Median time from ART prescription to controlled VL 
get.suppression.rate.france = function(){
    max.proportion = 0.95
    
    # rate = 1/time
    # proportion = 1 - exp(-rate*time)
    
    # 2010
    time = c(83)/365
    rate = 1/time
    annual.proportion = 1 - exp(-rate)
    data = annual.proportion
    
    dim.names.data = list(year = c(2010),
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
              slope = 0, # fit$coefficients[2], # NO SLOPE FOR SINGLE YEAR ESTIMATE 
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
