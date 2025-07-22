

# Lay 2017: 2003-2013, non-governmental hospital in Phnom Penh
get.disengagement.model.cambodia = function(){
    disengagement.p = (.06) # 6% LTFU at one year 
    
    disengagement.rate.unsuppressed = -log(1-disengagement.p)
    disengagement.rate.suppressed = disengagement.rate.unsuppressed
    
    rv = list()
    rv$disengagement.rate.unsuppressed = disengagement.rate.unsuppressed
    rv$disengagement.rate.suppressed = disengagement.rate.suppressed
    
    rv
    
}

# Vu et al, 2025 (TAHOD)
get.engagement.model.asia = function(){
    
    # Method 1: "The probability of not having initiated ART at two years from HIV diagnosis"
    time = 2
    prob = c((1-.1630), # 16.30% (95% CI: 15.17, 17.47) for those diagnosed in 2013–2015
             (1-.0895), # 8.95% (95% CI: 8.23, 9.69) for 2016–2019,
             (1-.1011)) # 10.11% (95% CI: 8.77, 11.55) after 2020.
    
    # rate = (-log(1-prob))/time
    # prob = 1 - exp(-rate*time)
    
    rate = (-log(1-prob))/time
    annual.proportion = 1 - exp(-rate)
    data = c(0,annual.proportion)

    # Method 2: Median time to ART initiation - this yields WAY too high values; using other method 
    if(1==2){
        time = c(51,28,26)/365 # 51 days (2013–2015), 28 days (2016–2019), and 26 days (≥ 2020). 
        rate = 1/time
        annual.proportion = 1 - exp(-rate)
        data = c(0,annual.proportion) 
    }
    
    dim.names.data = list(year = c(1990,2014,2017,2021),
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