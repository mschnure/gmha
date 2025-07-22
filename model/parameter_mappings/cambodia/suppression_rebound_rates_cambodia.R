
# Barennes, 2014: 2010–2012 in the HIV clinic of Calmette Hospital located in Phnom Penh
get.unsuppression.rate.cambodia = function(){
    
    prob = c(.06) #  6.0% 12 months VF rate of naive patients 
    time = c(1)
    
    rate = (-log(1-prob))/time
    #prob = 1 - exp(-rate*time)
    
    time.to.rebound = 1/rate # 16.16151 years  

    rate
    
}


# suppression - using Thailand model 