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
