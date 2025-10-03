
# CALCULATING ANNUAL INTERVENTION RATES --> final value = 0.06497952
# (A) Fraction who choose LAI * (B) fraction who get virally suppressed * (C) rate of suppression

# (A) Fraction who choose LAI
# Start from 10% over 5 years, back-calculate to what proportion accept per year 
# rate = (-log(1-prob))/time
# prob = 1 - exp(-rate*time)
prob.5.year = 0.10
rate.5.year = (-log(1-prob.5.year))/5
prob.1.year = 1 - exp(-rate.5.year*1)
#rate.1.year = (-log(1-prob.1.year))/1 # this should be the exact same as rate.5.year, just checking 
annual.fraction.accepting.lai = prob.1.year # 0.02085164


# (B) Fraction who get virally suppressed
# per AFINAty poster: 
# "172 participants were enrolled across 3 cohorts, of which 134 were virally suppressed after screening and switched to LAI."
# don't have any differences by cohort 
fraction.gaining.suppression = 134/172 # 0.7790698

# (C) Rate of suppression
# this can be different for the three cohorts I think
# assuming 3 months to gain suppression --> rate = 4
lai.suppression.rate = 4
if(1==2){
    # review baseline rate of suppression 
    sa.supp.prop = get.suppression.rates("South Africa")
    sa.supp.prop = sa.supp.prop$predictions
    sa.supp.prop = apply(sa.supp.prop,"year",mean) # these are proportions, converted to rates in map.model.parameters code 
    sa.supp.rates = -log(1-sa.supp.prop)
    cbind(sa.supp.rates) # rate of ~3 in modern era     
}

# ANNUAL RATE = 0.06497952 (assuming 10% uptake over 5 years)
annual.lai.suppression.rate = annual.fraction.accepting.lai * fraction.gaining.suppression * lai.suppression.rate # 0.06497952



# BASELINE RATES (all 0, plugged into parameters code)
get.lai.es.art.rates = function(location = "South Africa"){
    rv = list("annual.lai.rate" = 0.0)
    
    rv
}

get.lai.eu.art.rates = function(location = "South Africa"){
    rv = list("annual.lai.rate" = 0.0)
    
    rv
}


get.lai.du.art.rates = function(location = "South Africa"){
    rv = list("annual.lai.rate" = 0.0)
    
    rv
}