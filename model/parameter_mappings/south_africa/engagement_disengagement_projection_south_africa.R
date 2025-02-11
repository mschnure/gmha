
## Cornell et al, 2010: IeDEA network, 2002-2007 
get.disengagement.model.south.africa = function(){
    disengagement.p = 0.144 # % loss-to-follow-up at 12 months 
    
    disengagement.rate.unsuppressed = -log(1-disengagement.p)
    disengagement.rate.suppressed = disengagement.rate.unsuppressed
    
    rv = list()
    rv$disengagement.rate.unsuppressed = disengagement.rate.unsuppressed
    rv$disengagement.rate.suppressed = disengagement.rate.suppressed
    
    rv
}


# ART initiation within 12 months of enrollment in HIV care, in ART-naive patients
# Treatment Outcomes among Patients Enrolling in Care in Sub-Saharan Africa
# e.g., 53% of 10-19yo males in 2010 initiated ART within 12 months of enrollment in HIV care at an IeDEA site 

get.engagement.model.south.africa = function(){
    
    max.proportion = 0.95
    
    data = c(0, # art.start.male.10.19.1990 - fixed 
             .53, # art.start.male.10.19.2010 
             .52, # art.start.male.10.19.2014
             .91, # art.start.male.10.19.2017
             
             0, # art.start.male.20.29.1990 - fixed 
             .54, # art.start.male.20.29.2010
             .58, # art.start.male.20.29.2014
             .85, # art.start.male.20.29.2017
             
             0, # art.start.male.30.39.1990 - fixed 
             .76, # art.start.male.30.39.2010
             .71, # art.start.male.30.39.2014
             .93, # art.start.male.30.39.2017
             
             0, # art.start.male.40.49.1990 - fixed 
             .82, # art.start.male.40.49.2010
             .78, # art.start.male.40.49.2014
             .94, # art.start.male.40.49.2017
             
             0, # art.start.male.50plus.1990 - fixed 
             .81, # art.start.male.50plus.2010
             .82, # art.start.male.50plus.2014
             .95, # art.start.male.50plus.2017  

             0, # art.start.female.10.19.1990 - fixed 
             .29, # art.start.female.10.19.2010
             .40, # art.start.female.10.19.2014
             .78, # art.start.female.10.19.2017
             
             0, # art.start.female.20.29.1990 - fixed 
             .62, # art.start.female.20.29.2010
             .65, # art.start.female.20.29.2014
             .87, # art.start.female.20.29.2017
             
             0, # art.start.female.30.39.1990 - fixed 
             .79, # art.start.female.30.39.2010
             .74, # art.start.female.30.39.2014
             .91, # art.start.female.30.39.2017
             
             0, # art.start.female.40.49.1990 - fixed 
             .79, # art.start.female.40.49.2010
             .74, # art.start.female.40.49.2014
             .93, # art.start.female.40.49.2017
             
             0, # art.start.female.50plus.1990 - fixed 
             .82, # art.start.female.50plus.2010
             .75, # art.start.female.50plus.2014
             .94 # art.start.female.50plus.2017  
             )
    
    dim.names.data = list(year = c(1990,2010,2014,2017),
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


