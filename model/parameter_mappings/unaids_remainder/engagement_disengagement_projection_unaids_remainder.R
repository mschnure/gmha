


##----------------------------------------------##
##-- Engagement estimated from AIDS info data --##
##----------------------------------------------##
get.engagement.model.unaids = function(){
    # To get engagement proportion, take (# start art)/(# off art)
    # But first, start with annual difference (will help solve for # start art below)
    
    on.art = read.csv("data_manager/data/engagement_ART/Treatment cascade_People living with HIV receiving ART (#)_Population_ All ages.csv")
    on.art = as.numeric(gsub(" ","",on.art[nrow(on.art),-1]))
    on.art = on.art[!is.na(on.art)]
    
    years = c(2010:2021)
    dim.names = list(year = years)
    
    on.art = array(on.art,
                   dim = sapply(dim.names, length),
                   dimnames = dim.names)
    
    annual.difference=0
    # annual difference = (year2 on art) - (year1 on art) 
    for(i in 1:length(years)-1){
        annual.difference[i] = on.art[i+1] - on.art[i]
        names(annual.difference)[i] = paste0(names(on.art[i+1]),"-",names(on.art[i]))
    }
    
    # Solving for start.art,
    # annual.difference = start.art - stop.art
    # annual.difference + stop.art = start.art
    disengagement.p = 0.13 # FROM LEE ET AL ABOVE
    stop.art = on.art*disengagement.p
    start.art = annual.difference + stop.art[-length(stop.art)]
    names(start.art) = as.character(2011:2021)
    
    # Solving for off.art
    # on.art = total.plhiv*percent.on.art
    # total.plhiv = on.art/percent.on.art
    # (on.art + off.art) = (on.art/percent.on.art)
    # off.art = (on.art/percent.on.art) - on.art
    percent.on.art = read.csv("data_manager/data/engagement_ART/Treatment cascade_People living with HIV receiving ART (%)_Population_ All ages.csv")
    percent.on.art = as.numeric(gsub(">","",percent.on.art[nrow(percent.on.art),c(2+3*c(0:11))])) # pull out the national row, median estimates 
    percent.on.art = percent.on.art[!is.na(percent.on.art)]
    
    percent.on.art = array(percent.on.art/100,
                           dim = sapply(dim.names, length),
                           dimnames = dim.names)
    
    dim.names.data = list(year = years,
                          age = c("10-19","20-29","30-39","40-49","50 and over"),
                          sex = c("male","female"))
    
    data.array = array(percent.on.art,
                       dim = sapply(dim.names.data, length),
                       dimnames = dim.names.data)
    
    off.art = (on.art/percent.on.art) - on.art
    
    # ENGAGEMENT RATE = start.art/off.art
    engagement.rate = start.art/off.art[-length(off.art)] 
    engagement.anchor.year = 1990
    engagement.years = as.numeric(names(engagement.rate))-engagement.anchor.year
    
    df = data.frame(value=engagement.rate,
                    year=engagement.years)
    
    fit = suppressWarnings(glm(value~year, family=binomial,data=df)) 
    max.proportion = 0.95
    
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
              slope=fit$coefficients[2],
              anchor.year=engagement.anchor.year,
              max.proportion=max.proportion,
              df=df,
              engagement.rate = engagement.rate,
              data.array = data.array,
              predictions=predictions)
    
    rv
    
}

# this one has different slopes for 2016 and 2017 
get.engagement.model.unaids.multi.slope = function(){
    # To get engagement proportion, take (# start art)/(# off art)
    # But first, start with annual difference (will help solve for # start art below)
    
    on.art = read.csv("data_manager/data/engagement_ART/Treatment cascade_People living with HIV receiving ART (#)_Population_ All ages.csv")
    on.art = as.numeric(gsub(" ","",on.art[nrow(on.art),-1]))
    on.art = on.art[!is.na(on.art)]
    
    years = c(2010:2021)
    dim.names = list(year = years)
    
    on.art = array(on.art,
                   dim = sapply(dim.names, length),
                   dimnames = dim.names)
    
    annual.difference=0
    # annual difference = (year2 on art) - (year1 on art) 
    for(i in 1:length(years)-1){
        annual.difference[i] = on.art[i+1] - on.art[i]
        names(annual.difference)[i] = paste0(names(on.art[i+1]),"-",names(on.art[i]))
    }
    
    # Solving for start.art,
    # annual.difference = start.art - stop.art
    # annual.difference + stop.art = start.art
    disengagement.p = 0.13 # FROM LEE ET AL ABOVE
    stop.art = on.art*disengagement.p
    start.art = annual.difference + stop.art[-length(stop.art)]
    names(start.art) = as.character(2011:2021)
    
    
    # Solving for off.art
    # on.art = total.plhiv*percent.on.art
    # total.plhiv = on.art/percent.on.art
    # (on.art + off.art) = (on.art/percent.on.art)
    # off.art = (on.art/percent.on.art) - on.art
    percent.on.art = read.csv("data_manager/data/engagement_ART/Treatment cascade_People living with HIV receiving ART (%)_Population_ All ages.csv")
    percent.on.art = as.numeric(gsub(">","",percent.on.art[nrow(percent.on.art),c(2+3*c(0:11))])) # pull out the national row, median estimates 
    percent.on.art = percent.on.art[!is.na(percent.on.art)]
    
    percent.on.art = array(percent.on.art/100,
                           dim = sapply(dim.names, length),
                           dimnames = dim.names)
    
    dim.names.data = list(year = years,
                          age = c("10-19","20-29","30-39","40-49","50 and over"),
                          sex = c("male","female"))
    
    data.array = array(percent.on.art,
                       dim = sapply(dim.names.data, length),
                       dimnames = dim.names.data)
    
    off.art = (on.art/percent.on.art) - on.art
    
    # ENGAGEMENT RATE = start.art/off.art
    engagement.rate = start.art/off.art[-length(off.art)] 
    engagement.anchor.year = 2000
    engagement.years = as.numeric(names(engagement.rate))-engagement.anchor.year
    
    df = data.frame(y=engagement.rate,
                    year=engagement.years)
    df$year.2 = pmax(0,df$year-15)
    df$year.4 = pmax(0,df$year-15) 
    df$year.4[df$year>17]=0 # makes a 1 in 2016 and 2 in 2017, all the rest are zero 
    
    fit1 = suppressWarnings(glm(y~year+year.2, family=binomial,data=df)) # logistic with spline at 2017 (family=binomial, meaning link is logit)
    fit2 = suppressWarnings(glm(y~year+year.2+year.4, family=binomial,data=df)) # three slopes: pre-2016, 2016-2017, post 2017 - but I don't know if this is true actually
    
    fit.to.use = "fit2"
    
    max.proportion = 0.95
    
    dim.names.projection = list(year = 1980:2040 - engagement.anchor.year,
                                age = c("10-19","20-29","30-39","40-49","50 and over"),
                                sex = c("male","female"))

    projection.array = array(NA,
                             dim = sapply(dim.names.projection,length),
                             dimnames = dim.names.projection)
    
    projection.array[as.character(dim.names.data$year - engagement.anchor.year),,] = data.array
    
    projection.df = reshape2::melt(projection.array)
    projection.df$year.2 = pmax(0,projection.df$year-15)
    projection.df$year.4 = pmax(0,projection.df$year-15) 
    projection.df$year.4[df$year>17]=0 # makes a 1 in 2016 and 2 in 2017, all the rest are zero 
    
    if(fit.to.use=="fit1"){
        fit=fit1
        
        predictions = predict(fit,newdata=projection.df,type="response") # predict off of model (same as multiplying coefficients)
        predictions = predictions*max.proportion
        dim(predictions) = sapply(dim.names.projection,length)
        dimnames(predictions) = dim.names.projection
        
        dimnames(predictions)$year = as.numeric(dimnames(predictions)$year) + engagement.anchor.year
        
        rv = list(intercept=fit$coefficients[1],
                  pre.universal.slope=fit$coefficients[2],
                  post.universal.slope=fit$coefficients[3],
                  anchor.year=engagement.anchor.year,
                  max.proportion=max.proportion,
                  df=df,
                  engagement.rate = engagement.rate,
                  data.array = data.array,
                  predictions=predictions)
    }
    
    if(fit.to.use=="fit2"){
        fit=fit2
        
        predictions = predict(fit,newdata=projection.df,type="response") # predict off of model (same as multiplying coefficients)
        predictions = predictions*max.proportion
        dim(predictions) = sapply(dim.names.projection,length)
        dimnames(predictions) = dim.names.projection
        
        dimnames(predictions)$year = as.numeric(dimnames(predictions)$year) + engagement.anchor.year
        
        rv = list(intercept=fit$coefficients[1],
                  pre.universal.slope=fit$coefficients[2],
                  intermediate.slope.2016.2017=fit$coefficients[4],
                  post.universal.slope=fit$coefficients[3],
                  anchor.year=engagement.anchor.year,
                  max.proportion=max.proportion,
                  df=df,
                  engagement.rate = engagement.rate,
                  data.array = data.array,
                  predictions=predictions)
    }
    
    rv
    
}
