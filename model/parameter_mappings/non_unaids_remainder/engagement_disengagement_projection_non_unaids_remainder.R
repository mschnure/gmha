
# average India and US estimates from below 
get.mean.non.unaids.disengagement.rate = function(){
    
    india.dinsengagement = get.disengagement.model.india()
    us.disengagement = get.disengagement.model.us()
    
    mean.disengagement = mean(c(india.dinsengagement$disengagement.rate.unsuppressed,
                              us.disengagement$disengagement.rate.unsuppressed))
    
    rv = list()
    rv$disengagement.rate.unsuppressed = mean.disengagement
    rv$disengagement.rate.suppressed = mean.disengagement
    
    rv
}

## India disengagement - Blutinger et al 2014: https://www.tandfonline.com/doi/full/10.1080/09540121.2014.934654
get.disengagement.model.india = function(){
    disengagement.rate.one.year = .381
    
    disengagement.prob.one.year = 1-exp(-(disengagement.rate.one.year))
    #disengagement.prob.one.year # 0.3168221
    
    time.to.disengagement = 1/disengagement.rate.one.year
    time.to.disengagement # 2.624672

    rv = list()
    rv$disengagement.rate.unsuppressed = disengagement.rate.one.year
    rv$disengagement.rate.suppressed = disengagement.rate.one.year
    
    rv

}

## US disengagement - Yehia et al, 2012: 
# https://journals.lww.com/aidsonline/FullText/2012/06010/Comparing_different_measures_of_retention_in.9.aspx
get.disengagement.model.us = function(){
    disengagement.p = 0.25 # ~75% retention; this is a very rough estimate 
    
    disengagement.rate.unsuppressed = -log(1-disengagement.p)
    disengagement.rate.suppressed = disengagement.rate.unsuppressed
    
    rv = list()
    rv$disengagement.rate.unsuppressed = disengagement.rate.unsuppressed
    rv$disengagement.rate.suppressed = disengagement.rate.suppressed
    
    rv

}
