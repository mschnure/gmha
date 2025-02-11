source("model/parameter_mappings/kenya/suppression_rebound_rates_kenya.R")
source("model/parameter_mappings/south_africa/suppression_rebound_rates_south_africa.R")

get.suppression.rates = function(location){
    
    if(location=="South Africa"){
        rv = get.suppression.rate.south.africa() 
    } else {
        rv = get.suppression.rate.kenya() # use KENYA'S suppression data for all other countries 
    }
    rv
    
}

get.unsuppression.rates = function(location){
    
    if(location=="South Africa"){
        rv = get.unsuppression.rate.south.africa() 
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
        model = get.suppression.rate.kenya.iedea()
    } else 
        stop("can only plot for South Africa and Kenya")
    
    
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
