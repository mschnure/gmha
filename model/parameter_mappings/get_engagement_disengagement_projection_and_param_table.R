source("model/parameter_mappings/kenya/engagement_disengagement_projection_kenya.R")
source("model/parameter_mappings/south_africa/engagement_disengagement_projection_south_africa.R")
source("model/parameter_mappings/tanzania/engagement_disengagement_projection_tanzania.R")
source("model/parameter_mappings/uganda/engagement_disengagement_projection_uganda.R")
source("model/parameter_mappings/zimbabwe/engagement_disengagement_projection_zimbabwe.R")
source("model/parameter_mappings/nigeria/engagement_disengagement_projection_nigeria.R")
source("model/parameter_mappings/non_unaids_remainder/engagement_disengagement_projection_non_unaids_remainder.R")
source("model/parameter_mappings/france/engagement_disengagement_projection_france.R")
source("model/parameter_mappings/thailand/engagement_disengagement_projection_thailand.R")

# estimates here are proportions (data are reported as probabilities of engaging within a year)
# converted to a rate in the map.model.parameters code 
get.engagement.model = function(location){
    
    if(location=="Kenya"){
        rv = get.engagement.model.kenya() 
    } else if(location=="Tanzania"){
        rv = get.engagement.model.tanzania()
    } else if(location=="Uganda"){
        rv = get.engagement.model.uganda()
    } else if(location=="Zimbabwe"){
        rv = get.engagement.model.zimbabwe()
    } else if(location=="Nigeria"){
        rv = get.engagement.model.nigeria()
    } else if(location=="non.unaids.remainder"){
        rv = get.engagement.model.india()
    } else if(location=="France"){
        rv = get.engagement.model.france()
    } else if(location=="Thailand"){
        rv = get.engagement.model.thailand()
    } else {
        rv = get.engagement.model.south.africa() 
        # use SOUTH AFRICA'S engagement data for all other countries (Moz, Zambia, Malawi)
        #print("Using South Africa's engagement model")
    }
    rv
}

# THIS ISN'T ACTUALLY USED ANYWHERE; PLUGGED IN DIRECTLY INTO PARAMETERS AND PRIOR
# estimates here are RATES (data are reported as probabilities) 
# converted to rate within the get.disengagement.model.functions)
get.disengagement.model = function(location){
    
    if(location=="South Africa"){
        rv = get.disengagement.model.south.africa() 
    } else if(location=="non.unaids.remainder"){
        get.mean.non.unaids.disengagement.rate() # using mean of India and US disengagement data for non-UNAIDS remainder
    } else if(location=="France"){
        get.disengagement.model.france() 
    } else if(location=="Thailand"){
        get.disengagement.model.thailand() 
    } else {
        rv = get.disengagement.model.kenya() # use KENYA'S disengagement data for all other countries 
    }
    rv
    
}


plot.engagement.fit = function(location="South Africa",
                               dimension = "total" # or age or sex
){
    model = get.engagement.model(location=location)
    
    if(dimension=="age"){
        plot = ggplot() + 
            geom_point(data=reshape2::melt(apply(model$data.array, c("age","year"),mean)), aes(x=year, y=value, color=age)) + 
            geom_line(data=reshape2::melt(apply(model$predictions, c("age","year"),mean)), aes(x=year, y=value, color=age)) + 
            ylim(0,1) + facet_wrap(~age) 
    } else if(dimension=="sex"){
        plot = ggplot() + 
            geom_point(data=reshape2::melt(apply(model$data.array, c("sex","year"),mean)), aes(x=year, y=value, color=sex)) + 
            geom_line(data=reshape2::melt(apply(model$predictions, c("sex","year"),mean)), aes(x=year, y=value, color=sex)) + 
            ylim(0,1) + facet_wrap(~sex) 
    } else if(dimension=="total"){
        data = reshape2::melt(apply(model$data.array, c("year"),mean))
        data$year = as.numeric(rownames(data))
        
        predictions = reshape2::melt(apply(model$predictions, c("year"),mean))
        predictions$year = as.numeric(rownames(predictions))
        
        plot = ggplot() + 
            geom_point(data=data, aes(x=year, y=value)) + 
            geom_line(data=predictions, aes(x=year, y=value)) + 
            ylim(0,1) 
    } else  
        stop("can only plot engagement fit by total, age, or sex dimension")
    
    plot
    
    
}

# THIS IS OLD 
generate.engagement.parameter.table = function(locations){
    
    rv = list()
    
    for(location in locations){
        model = get.engagement.model(location=location)
        df = model$df
        max.proportion = model$max.proportion
        
        odds = exp(model$intercept + model$pre.universal.slope*15)
        lower.odds = odds/4
        upper.odds = odds*4
        p.engage.2015 = odds/(1+odds)
        
        # Probability of engaging, 2015
        p.engage.2015.lower = (odds/(1+odds))/4
        p.engage.2015.upper = (odds/(1+odds))*4
        
        # pre-2015 slope 
        pre.2015.OR = exp(model$pre.universal.slope)
        pre.2015.OR.lower = pre.2015.OR/4
        pre.2015.OR.upper = pre.2015.OR*4
        
        # 2016-2017 slope 
        universal.ART.OR = exp(model$pre.universal.slope +  model$post.universal.slope + model$intermediate.slope.2016.2017)
        universal.ART.OR.lower = universal.ART.OR/4
        universal.ART.OR.upper = universal.ART.OR*4
        
        # post-2017 slope 
        post.2017.OR = exp(model$pre.universal.slope +  model$post.universal.slope)
        post.2017.OR.lower = post.2017.OR/4
        post.2017.OR.upper = post.2017.OR*4
        
            table = data.frame(
            Parameter = c("Probability of engaging, Female, 2015",
                          "Yearly change in odds of starting ART prior to universal ART, 2016 (odds ratio)",
                          "Yearly change in odds of starting ART during rollout of universal ART, 2016–2017 (odds ratio)",
                          "Yearly change in odds of starting ART after initial rollout of universal ART, 2017 (odds ratio)",
                          "Ratio of engagement rates among male versus female"),
            
            Estimate.lower.upper=c(paste0(round(p.engage.2015,2), " (",round(p.engage.2015.lower,2),"-",
                                          round((p.engage.2015.upper),2),")"),
                                   paste0(round(pre.2015.OR,2), " (",round(pre.2015.OR.lower,2),"-",
                                          round((pre.2015.OR.upper),2),")"),
                                   paste0(round(universal.ART.OR,2), " (",round(universal.ART.OR.lower,2),"-",
                                          round((universal.ART.OR.upper),2),")"),
                                   paste0(round(post.2017.OR,2), " (",round(post.2017.OR.lower,2),"-",
                                          round((post.2017.OR.upper),2),")"),
                                   paste0("1 (0.25-4)")))
        
        rv[[location]] = table
    }
    
    rv
    
}



plot.engagement.fit.kenya.old = function(location="Kenya"){
    engagement.rate = get.engagement.model.kenya()$engagement.rate
    engagement.years = as.numeric(names(engagement.rate))
    #qplot(names(engagement.rate),engagement.rate) + ylim(0,NA)
    
    df = data.frame(value=c(rep(NA,31),engagement.rate,rep(NA,30)),
                    year=c(1980:2010,engagement.years,2021:2050))
    
    fit = suppressWarnings(glm(value~year, family=binomial,data=df)) 
    
    predictions = predict(fit,newdata=df,type="response") # predict off of model (same as multiplying coefficients)
    
    predictions = data.frame(value=predictions,
                             year=df$year)
    
    plot = ggplot() + 
        geom_point(data=df, aes(x=year, y=value)) + 
        geom_line(data=predictions, aes(x=year, y=value)) + 
        ylim(0,1)
    
    plot
}


plot.engagement.fit.kenya.old = function(location="Kenya"){
    engagement.rate = get.engagement.model.kenya.old()$engagement.rate
    engagement.years = as.numeric(names(engagement.rate))
    #qplot(names(engagement.rate),engagement.rate) + ylim(0,NA)
    
    df = data.frame(y=c(rep(NA,31),engagement.rate,rep(NA,30)),
                    x=c(1980:2010,engagement.years,2021:2050))
    
    # spline options: 
    df$x2 = pmax(0,df$x-2016) # years only at/after 2016 (pmax makes sure it isn't negative)
    df$x3 = pmax(0,df$x-2017) # same but for 2017
    df$x4 = pmax(0,df$x-2015) 
    df$x4[df$x>2017]=0 # makes a 1 in 2016 and 2 in 2017, all the rest are zero 
    df$x5 = as.numeric(df$x==2016 | df$x==2017) # rather than a separate slope from 2016-2017 as in previous, just a one-time bump
    
    fit1 = lm(y ~ x, data = df) # linear, no spline
    fit2 = lm(y~x+x2, data=df) # linear, spline 
    fit3 = glm(y~x+x2, family=binomial,data=df) # logit, spline - wouldn't have runaway exponential growth **
    fit4 = glm(y~x+x2, family=poisson,data=df) # log, spline - makes sense for a rate, but could have runaway growth
    fit5 = glm(y~x, family=binomial,data=df) # logit link without spline - limits runaway growth more than log link w/o spline, but not as good as spline
    fit6 = glm(y~x, family=poisson,data=df) # log link without spline - runaway growth
    fit7 = glm(y~x+x3, family=poisson,data=df) # with 2018 spline point, will be negative slope
    fit8 = glm(y~x+x4, family=binomial,data=df) # with separate slope in 2016/2017
    fit9 = glm(y~x+x2+x4, family=binomial,data=df) # one slope for early, another slope for 2016/2017, third slope for after
    fit10 = glm(y~x+x5, family=binomial,data=df) # bump in 2016/2017 
    
    fit=fit9
    
    predictions = predict(fit,newdata=df,type="response") # predict off of model (same as multiplying coefficients)
    
    predictions = data.frame(y=predictions,
                             x=df$x)
    
    plot = ggplot() + 
        geom_point(data=df, aes(x=x, y=y)) + 
        geom_line(data=predictions, aes(x=x, y=y)) + 
        ylim(0,1)
    
    plot
}

