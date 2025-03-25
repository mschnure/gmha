


get.unsuppression.rate.south.africa = function(){
 
    ## Hermans et al, 2020 (52 centers across South Africa; 2007-2018; ~100k patients)
    # Among those followed, viremia occurred in 19.8% of patients after median follow up of 152 weeks 
    prob = c(0.198)
    time = c(152/52)
    # --> rate = 0.07548439 --> time to rebound = 13.24777 years 
    
    rate = (-log(1-prob))/time
    prob = 1 - exp(-rate*time)
    
    time.to.rebound = 1/rate
    
    if(1==2){
        ## Sanne et al, 2009 (Johannesburg clinical cohort; 2004-2008; ~7k patients)
        # Among those virally suppressed, p.rebound = 
        # 1 year: 9.4% --> rate = 0.09871597 --> time to rebound = 10.13 years 
        # 2 years: 16.8% --> rate = 0.09196142 --> time to rebound = 10.87 years
        # 3 years: 20.6% --> rate = 0.07689061 --> time to rebound = 13 years
        prob = c(0.094,0.168,0.206)
        time = c(1,2,3)
        
        rate = (-log(1-prob))/time
        prob = 1 - exp(-rate*time)
        
        time.to.rebound = 1/rate
    }

    rate
    
}



# Viral load suppression among patients 12 months after initiating ART, among those who had a viral load test recorded at 12 months after ART initiation
# "Treatment Outcomes among Patients Initiating ART in Sub-Saharan Africa"
get.suppression.rate.south.africa = function(){
    max.proportion = 0.95

    data = suppressMessages(readxl::read_xlsx("data_manager/IeDEA_data.xlsx",sheet="supp_sa",col_names=T))
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
