

# average India and US estimates from below 
get.mean.non.unaids.unsuppression.rate = function(){
    
    india.unsuppression = get.unsuppression.rate.india()
    us.unsuppression = get.unsuppression.rate.us()
    
    mean.unsuppression = mean(c(india.unsuppression,
                                us.unsuppression))
    
    mean.unsuppression
}

## Dinesha et al, 2024: https://onlinelibrary.wiley.com/doi/full/10.1111/hiv.13641 
# Among those followed, viremia occurred in 10% of patients after median follow up of 2.2 years
get.unsuppression.rate.india = function(){
 
    prob = c(0.10)
    time = c(2.2)
    # --> rate = 0.04789114 --> time to rebound = 20.88069 years 
    
    rate = (-log(1-prob))/time
    prob = 1 - exp(-rate*time)
    
    time.to.rebound = 1/rate

    rate
    
}


# Yehia et al, 2015: https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0129376
# 7.4% of retained moved from suppressed to not suppressed in one year 
get.unsuppression.rate.us = function(){
    
    prob = c(0.074)
    time = 1
    # --> rate = 0.07688104 --> time to rebound = 13.00711 years 
    
    rate = (-log(1-prob))/time
    prob = 1 - exp(-rate*time)
    
    time.to.rebound = 1/rate
    
    rate
    
}

