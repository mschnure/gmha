
##-------------------------------------------##
##-- Disengagement - Lee et al 2018 AMPATH --##
##-------------------------------------------##
get.disengagement.model.kenya = function(){
    disengagement.p = 0.13
    disengagement.rate.unsuppressed = -log(1-disengagement.p)
    
    # Relative risks of disengaging, both relative to CD4 below 350, off ART 
    RRR.CD4.below.350.on.ART = .16
    RRR.CD4.above.350.on.ART = .12
    
    # Assume most in cohort were unsuppressed since only 12.8% taking ART at enrollment
    suppressed.vs.unsuppressed.disengagement = RRR.CD4.above.350.on.ART/RRR.CD4.below.350.on.ART
    
    disengagement.p.suppressed = disengagement.p*suppressed.vs.unsuppressed.disengagement
    disengagement.rate.suppressed = -log(1-disengagement.p.suppressed)
    
    rv = list()
    rv$disengagement.rate.unsuppressed = disengagement.rate.unsuppressed
    rv$disengagement.rate.suppressed = disengagement.rate.suppressed
    
    rv
}



# ART initiation within 12 months of enrollment in HIV care, in ART-naive patients
# Treatment Outcomes among Patients Enrolling in Care in Sub-Saharan Africa
get.engagement.model.kenya = function(){
    
    max.proportion = 0.95
    
    data = suppressMessages(readxl::read_xlsx("data_manager/IeDEA_data.xlsx",sheet="eng_ken",col_names=T))
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


##----------------------------------------------------##
##-- Engagement OLD - estimated from AIDS info data --##
##----------------------------------------------------##
get.engagement.model.kenya.old.2 = function(){
    # To get engagement proportion, take (# start art)/(# off art)
    # But first, start with annual difference (will help solve for # start art below)
    
    on.art = read.csv("data_manager/data/engagement_ART/kenya/Treatment cascade_People living with HIV receiving ART (#)_Population_ All ages.csv")
    on.art = as.numeric(gsub(" ","",on.art[nrow(on.art),-1]))
    
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
    percent.on.art = read.csv("data_manager/data/engagement_ART/kenya/Treatment cascade_People living with HIV receiving ART (%)_Population_ All ages.csv")
    percent.on.art = as.numeric(gsub(">","",percent.on.art[nrow(percent.on.art),c(2+3*c(0:11))])) # pull out the national row, median estimates 
    
    percent.on.art = array(percent.on.art/100,
                           dim = sapply(dim.names, length),
                           dimnames = dim.names)
    
    off.art = (on.art/percent.on.art) - on.art
    
    # ENGAGEMENT RATE = start.art/off.art
    engagement.rate = start.art/off.art[-length(off.art)] 
    engagement.anchor.year = 1990
    engagement.years = as.numeric(names(engagement.rate))-engagement.anchor.year
    
    df = data.frame(value=engagement.rate,
                    year=engagement.years)

    fit = suppressWarnings(glm(value~year, family=binomial,data=df)) 
    max.proportion = 0.95
    
    rv = list(intercept=fit$coefficients[1],
              slope=fit$coefficients[2],
              anchor.year=engagement.anchor.year,
              max.proportion=max.proportion,
              df=df,
              engagement.rate = engagement.rate)

    rv
    
}

# this one has different slopes for 2016 and 2017 
get.engagement.model.kenya.old = function(){
    # To get engagement proportion, take (# start art)/(# off art)
    # But first, start with annual difference (will help solve for # start art below)
    
    on.art = read.csv("data_manager/data/engagement_ART/kenya/Treatment cascade_People living with HIV receiving ART (#)_Population_ All ages.csv")
    on.art = as.numeric(gsub(" ","",on.art[nrow(on.art),-1]))
    
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
    percent.on.art = read.csv("data_manager/data/engagement_ART/kenya/Treatment cascade_People living with HIV receiving ART (%)_Population_ All ages.csv")
    percent.on.art = as.numeric(gsub(">","",percent.on.art[nrow(percent.on.art),c(2+3*c(0:11))])) # pull out the national row, median estimates 
    
    percent.on.art = array(percent.on.art/100,
                           dim = sapply(dim.names, length),
                           dimnames = dim.names)
    
    off.art = (on.art/percent.on.art) - on.art
    
    
    
    # ENGAGEMENT RATE = start.art/off.art
    engagement.rate = start.art/off.art[-length(off.art)] 
    engagement.anchor.year = 2000
    engagement.years = as.numeric(names(engagement.rate))-engagement.anchor.year
    
    df = data.frame(y=engagement.rate,
                    x=engagement.years)
    df$x2 = pmax(0,df$x-15)
    df$x4 = pmax(0,df$x-15) 
    df$x4[df$x>17]=0 # makes a 1 in 2016 and 2 in 2017, all the rest are zero 
    
    fit1 = suppressWarnings(glm(y~x+x2, family=binomial,data=df)) # logistic with spline at 2017 (family=binomial, meaning link is logit)
    fit2 = suppressWarnings(glm(y~x+x2+x4, family=binomial,data=df)) # three slopes: pre-2016, 2016-2017, post 2017 - but I don't know if this is true actually
    
    fit.to.use = "fit2"
    
    max.proportion = 0.95
    
    if(fit.to.use=="fit1"){
        fit=fit1
        rv = list(intercept=fit$coefficients[1],
                  pre.universal.slope=fit$coefficients[2],
                  post.universal.slope=fit$coefficients[3],
                  anchor.year=engagement.anchor.year,
                  max.proportion=max.proportion,
                  df=df,
                  engagement.rate = engagement.rate)
    }
    
    if(fit.to.use=="fit2"){
        fit=fit2
        rv = list(intercept=fit$coefficients[1],
                  pre.universal.slope=fit$coefficients[2],
                  intermediate.slope.2016.2017=fit$coefficients[4],
                  post.universal.slope=fit$coefficients[3],
                  anchor.year=engagement.anchor.year,
                  max.proportion=max.proportion,
                  df=df,
                  engagement.rate = engagement.rate)
    }
    
    rv
    
}

