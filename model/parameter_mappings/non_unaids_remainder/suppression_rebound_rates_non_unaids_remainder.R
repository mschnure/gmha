

# average India and US estimates from below 
get.mean.non.unaids.unsuppression.rate = function(){
    
    india.unsuppression = get.unsuppression.rate.india()
    us.unsuppression = get.unsuppression.rate.us()
    
    mean.unsuppression = mean(c(india.unsuppression,
                                us.unsuppression))
    
    mean.unsuppression
}

## Dinesha et al, 2024: https://onlinelibrary.wiley.com/doi/full/10.1111/hiv.13641 
# Among those followed, viremia occurred in 10% of patients after median follow up of 2.2 years
get.unsuppression.rate.india = function(){
 
    prob = c(0.10)
    time = c(2.2)
    # --> rate = 0.04789114 --> time to rebound = 20.88069 years 
    
    rate = (-log(1-prob))/time
    prob = 1 - exp(-rate*time)
    
    time.to.rebound = 1/rate

    rate
    
}


# Yehia et al, 2015: https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0129376
# 7.4% of retained moved from suppressed to not suppressed in one year 
get.unsuppression.rate.us = function(){
    
    prob = c(0.074)
    time = 1
    # --> rate = 0.07688104 --> time to rebound = 13.00711 years 
    
    rate = (-log(1-prob))/time
    prob = 1 - exp(-rate*time)
    
    time.to.rebound = 1/rate
    
    rate
    
}


# Viral suppression within 6 months of ART initiation - Alvarez-Uria et al, 2013 (Maunank cites this paper)
# https://onlinelibrary.wiley.com/doi/10.1155/2013/384805
get.suppression.rate.india = function(){
    max.proportion = 0.95
    
    suppression.p.6.months = .818
    time = 6/12
    suppression.rate.1.year = (-log(1-suppression.p.6.months))/time
    suppression.p.1.year = 1-exp(-(suppression.rate.1.year))
    suppression.p.1.year # 0.966876 very high suppression within 1 year 
    
    data = suppression.p.1.year
    
    dim.names.data = list(year = c(1990,2010,2014,2017),
                          age = c("10-19","20-29","30-39","40-49","50 and over"),
                          sex = c("male","female"))
    
    data.array = array(data,
                       dim = sapply(dim.names.data, length),
                       dimnames = dim.names.data)
    
    data.array["1990",,]=0
    
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
    
    predictions = predict(fit,newdata=projection.df,type="response") # predict off of model (same as multiplying coefficients)
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
