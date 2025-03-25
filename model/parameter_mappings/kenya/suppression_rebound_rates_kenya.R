

##--------------------------------------##
##-- Viral rebound - Maina et al 2020 --##
##--------------------------------------##
get.unsuppression.rate.kenya = function(){
    rate.of.viral.rebound.per.100.person.months = mean(c(3.9,0.7,0.89))
    
    rate.of.viral.rebound.one.year = (rate.of.viral.rebound.per.100.person.months/100)*12
    
    viral.rebound.p.one.year = 1-exp(-(rate.of.viral.rebound.one.year))
    #viral.rebound.p.one.year # 0.1971601
    
    time.to.rebound = 1/rate.of.viral.rebound.one.year
    time.to.rebound # 4.553734
    
    ## 10/22/2024: realized that I used this probability of rebound (0.1971601) as a rate in the model and paper; 
    ## I should have used the direct rate (0.2196) and then the time to rebound would have been 4.55, not 5.07 - fix this in new model! 
    ## CORRECTED 12/23/2024
    
    rate.of.viral.rebound.one.year #0.2196
}


# Viral load suppression among patients 12 months after initiating ART, among those who had a viral load test recorded at 12 months after ART initiation
# "Treatment Outcomes among Patients Initiating ART in Sub-Saharan Africa"
get.suppression.rate.kenya = function(){
    max.proportion = 0.95
    
    data = suppressMessages(readxl::read_xlsx("data_manager/IeDEA_data.xlsx",sheet="supp_ken",col_names=T))
    data = suppressWarnings(as.numeric(unlist(c(data[2:5,2:6],data[9:12,2:6]))))
    
    dim.names.data = list(year = c(1990,2010,2014,2017),
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


## THID IS THE OLD SUPPRESSION METHOD; USING IEDEA ONE NOW 

##--------------------------------------##
##-- Suppression - Njuguna et al 2022 --##
##--------------------------------------##

# Suppression - Njuguna et al
# prop suppressed at 18 months
get.suppression.rate.kenya.old = function(){
    suppressed.p = 3227/(3227+741)
    suppressed.r = -log(1-suppressed.p)/(18/12)
    
    suppressed.p.12.months = 1-exp(-suppressed.r) # 0.6732885
    #-log(1-suppressed.p.12.months)
    
    time.to.suppress = 1/suppressed.r
    time.to.suppress # 0.8939124
    
    ## 10/22/2024: realized that I used this probability of suppression (0.6732885) as a rate in the prior distributions; 
    ## I should have used the direct rate (1.118678) and then the time to suppress would have been 0.8939124, not 1.49
    ## CORRECTED 12/23/2024
    
    suppressed.r # 1.118678
    
}





