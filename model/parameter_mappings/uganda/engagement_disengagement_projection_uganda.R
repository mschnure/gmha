

# ART initiation within 12 months of enrollment in HIV care, in ART-naive patients
# Treatment Outcomes among Patients Enrolling in Care in Sub-Saharan Africa
get.engagement.model.uganda = function(){
    
    max.proportion = 0.95
    
    data = suppressMessages(readxl::read_xlsx("data_manager/IeDEA_data.xlsx",sheet="eng_uga",col_names=T))
    data = suppressWarnings(as.numeric(unlist(c(data[2:5,2:6],data[9:12,2:6]))))

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


