source("model/parameter_mappings/kenya/suppression_rebound_rates_kenya.R")
source("model/parameter_mappings/south_africa/suppression_rebound_rates_south_africa.R")
source("model/parameter_mappings/non_unaids_remainder/suppression_rebound_rates_non_unaids_remainder.R")
source("model/parameter_mappings/france/suppression_rebound_rates_france.R")
source("model/parameter_mappings/thailand/suppression_rebound_rates_thailand.R")
source("model/parameter_mappings/cambodia/suppression_rebound_rates_cambodia.R")

# estimates here are proportions (data are reported as probabilities of suppressing within a year)
# converted to a rate in the map.model.parameters code 
get.suppression.rates = function(location){
    
    if(location %in% c("South Africa","Tanzania","Uganda","Zambia","Zimbabwe","Malawi","Nigeria")){
        rv = get.suppression.rate.south.africa() # using SA suppression data for these
    } else if(location=="non.unaids.remainder"){
        rv = get.suppression.rate.india() # using India for non.unaids.remainder
    } else if(location %in% c("unaids.remainder","r1.low","r1.lower.middle","r1.upper.middle","r1.high")){
        rv = get.suppression.rate.south.africa() # using South Africa for unaids.remainder
    } else if(location=="France"){
        rv = get.suppression.rate.france() 
    } else if(location=="Thailand"){
        rv = get.suppression.rate.thailand() 
    } else if(location=="Cambodia"){
        rv = get.suppression.rate.thailand() # using Thailand model for Cambodia as well 
    } else{
        rv = get.suppression.rate.kenya() # use KENYA'S suppression data for all other countries (Moz, France)
    }
    rv
    
}

# THIS ISN'T ACTUALLY USED ANYWHERE; PLUGGED IN DIRECTLY INTO PARAMETERS AND PRIOR
get.unsuppression.rates = function(location){
    
    if(location=="South Africa"){
        rv = get.unsuppression.rate.south.africa() 
    } else if(location=="non.unaids.remainder"){
        get.mean.non.unaids.unsuppression.rate() # using mean of India and US unsuppression data for non-UNAIDS remainder
    } else if(location=="France"){
        get.unsuppression.rate.france() 
    } else if(location=="Thailand"){
        get.unsuppression.rate.thailand() 
    } else if(location=="Cambodia"){
        get.unsuppression.rate.cambodia() 
    } else {
        rv = get.unsuppression.rate.kenya() # use KENYA'S unsuppression data for all other countries 
    }
    rv
    
}



plot.suppression.fit = function(location="South Africa",
                                dimension = "total" # or age or sex
){
    if(location=="South Africa"){
        model = get.suppression.rate.south.africa()    
    } else if(location=="Kenya"){
        model = get.suppression.rate.kenya() # using iedea one as the default now 
    } else if(location=="Thailand"){
        model = get.suppression.rate.thailand() 
    } else if(location=="non.unaids.remainder"){
        model = get.suppression.rate.india() 
    } else 
        stop("can only plot for South Africa, Kenya, and non.unaids.remainder")
    
    
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
        stop("can only plot suppression fit by total, age, or sex dimension")
    
    plot
    
    
}




generate.suppression.parameter.table = function(locations){
    
    rv = list()
    
    for(location in locations){
        suppression.model = get.suppression.rates(location=location)

        # Age 30-39, female 
        sex="female"
        age = "30-39"
        year = 2015
        
        log.odds.ref = suppression.model$intercept + 
            (suppression.model$slope*(year-suppression.model$anchor.year)) + 
            (suppression.model$sex.female*(sex=="female"))  
        #(suppression.model$age.10.19*(eng.cat=="10-19")) + 
        #(suppression.model$age.20.29*(eng.cat=="20-29")) + 
        #(suppression.model$age.40.49*(eng.cat=="40-49")) + 
        #(suppression.model$age.50.plus*(eng.cat=="50 and over")) + 
        
        odds.ref = exp(log.odds.ref)
        lower.odds.ref = odds.ref/4
        upper.odds.ref = odds.ref*4
        
        lower.prob.ref = (lower.odds.ref/(1+lower.odds.ref))*suppression.model$max.proportion
        upper.prob.ref = (upper.odds.ref/(1+upper.odds.ref))*suppression.model$max.proportion
        
        prob.ref = 1/(1+exp(-log.odds.ref)) 
        prob.ref = prob.ref*suppression.model$max.proportion
        
        # ORs by age
        age.10.19.OR = exp(suppression.model$age.10.19)  
        age.10.19.OR.lower = age.10.19.OR/4
        age.10.19.OR.upper = age.10.19.OR*4
        
        age.20.29.OR = exp(suppression.model$age.20.29)  
        age.20.29.OR.lower = age.20.29.OR/4
        age.20.29.OR.upper = age.20.29.OR*4
        
        age.40.49.OR = exp(suppression.model$age.40.49)  
        age.40.49.OR.lower = age.40.49.OR/4
        age.40.49.OR.upper = age.40.49.OR*4
        
        age.50.plus.OR = exp(suppression.model$age.50.plus)  
        age.50.plus.OR.lower = age.50.plus.OR/4
        age.50.plus.OR.upper = age.50.plus.OR*4
        
        table = data.frame(
            Parameter = c("Probability of gaining suppression, Female, age 30-39, 2015",
                          "Odds ratio of gaining suppression, age 10-19 vs 30-39",
                          "Odds ratio of gaining suppression, age 20-29 vs 30-39",
                          "Odds ratio of gaining suppression, age 40-49 vs 30-39",
                          "Odds ratio of gaining suppression, age 50 and over vs 30-39",
                          "Ratio of suppression probability among male versus female"),
            Estimate.lower.upper=c(paste0(round(prob.ref,2), " (",round(lower.prob.ref,2),"-",
                                          round(upper.prob.ref,2),")"),
                                   paste0(round(age.10.19.OR,2), " (",round(age.10.19.OR.lower,2),"-",
                                          round(age.10.19.OR.upper,2),")"),
                                   paste0(round(age.20.29.OR,2), " (",round(age.20.29.OR.lower,2),"-",
                                          round(age.20.29.OR.upper,2),")"),
                                   paste0(round(age.40.49.OR,2), " (",round(age.40.49.OR.lower,2),"-",
                                          round(age.40.49.OR.upper,2),")"),
                                   paste0(round(age.50.plus.OR,2), " (",round(age.50.plus.OR.lower,2),"-",
                                          round(age.50.plus.OR.upper,2),")"),
                                   paste0("1 (0.25-4)")))
        
        rv[[location]] = table
    }
    
    rv
    
}


