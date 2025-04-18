
# average India and US estimates from below 
get.mean.non.unaids.disengagement.rate = function(){
    
    india.dinsengagement = get.disengagement.model.india()
    us.disengagement = get.disengagement.model.us()
    
    mean.disengagement = mean(c(india.dinsengagement$disengagement.rate.unsuppressed,
                              us.disengagement$disengagement.rate.unsuppressed))
    
    rv = list()
    rv$disengagement.rate.unsuppressed = mean.disengagement
    rv$disengagement.rate.suppressed = mean.disengagement
    
    rv
}

## India disengagement - Blutinger et al 2014: https://www.tandfonline.com/doi/full/10.1080/09540121.2014.934654
get.disengagement.model.india = function(){
    disengagement.rate.one.year = .381
    
    disengagement.prob.one.year = 1-exp(-(disengagement.rate.one.year))
    #disengagement.prob.one.year # 0.3168221
    
    time.to.disengagement = 1/disengagement.rate.one.year
    time.to.disengagement # 2.624672

    rv = list()
    rv$disengagement.rate.unsuppressed = disengagement.rate.one.year
    rv$disengagement.rate.suppressed = disengagement.rate.one.year
    
    rv

}

## US disengagement - Yehia et al, 2012: 
# https://journals.lww.com/aidsonline/FullText/2012/06010/Comparing_different_measures_of_retention_in.9.aspx
get.disengagement.model.us = function(){
    disengagement.p = 0.25 # ~75% retention; this is a very rough estimate 
    
    disengagement.rate.unsuppressed = -log(1-disengagement.p)
    disengagement.rate.suppressed = disengagement.rate.unsuppressed
    
    rv = list()
    rv$disengagement.rate.unsuppressed = disengagement.rate.unsuppressed
    rv$disengagement.rate.suppressed = disengagement.rate.suppressed
    
    rv

}


# ART initiation within 3 months of determining eligibility - Alvarez-Uria et al, 2013 (Maunank cites this paper)
# https://onlinelibrary.wiley.com/doi/10.1155/2013/384805
get.engagement.model.india = function(){
    
    max.proportion = 0.95
    
    engagement.p.3.months = .673
    time = 3/12
    engagement.rate.1.year = (-log(1-engagement.p.3.months))/time
    engagement.p.1.year = 1-exp(-(engagement.rate.1.year))
    engagement.p.1.year # 0.9885662 very high engagement within 1 year 
    
    data = engagement.p.1.year
    
    dim.names.data = list(year = c(1990,2010,2014,2017),
                          age = c("10-19","20-29","30-39","40-49","50 and over"),
                          sex = c("male","female"))
    
    data.array = array(data,
                       dim = sapply(dim.names.data, length),
                       dimnames = dim.names.data)
    
    data.array["1990",,]=0
    
    data.df = reshape2::melt(data.array)
    data.df$age = factor(data.df$age, levels = c("30-39","10-19","20-29","40-49","50 and over"))
    
    engagement.anchor.year = 1990
    engagement.years = as.numeric(dim.names.data$year)-engagement.anchor.year
    
    data.df$year = engagement.years
    
    fit = suppressWarnings(glm(value ~ year+age+sex, family=binomial, data = data.df)) 
    
    dim.names.projection = list(year = 1980:2040 - engagement.anchor.year,
                                age = c("10-19","20-29","30-39","40-49","50 and over"),
                                sex = c("male","female"))
    
    projection.array = array(NA,
                             dim = sapply(dim.names.projection,length),
                             dimnames = dim.names.projection)
    
    projection.array[as.character(dim.names.data$year - engagement.anchor.year),,] = data.array
    
    projection.df = reshape2::melt(projection.array)
    
    predictions = predict(fit,newdata=projection.df,type="response") # predict off of model (same as multiplying coefficients)
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

