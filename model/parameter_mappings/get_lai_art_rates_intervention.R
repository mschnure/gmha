
# CALCULATING ANNUAL INTERVENTION RATES --> final value = 0.06497952
# (A) Fraction who choose LAI * (B) fraction who get virally suppressed * (C) rate of suppression

# (A) Fraction who choose LAI
# Start from X% over 5 years (set in create_lai_interventions.R ), back-calculate to what proportion accept per year 
# rate = (-log(1-prob))/time
# prob = 1 - exp(-rate*time)
prob.5.year = PROB.5.YEAR # set this in "create_lai_interventions.R"
rate.5.year = (-log(1-prob.5.year))/5
prob.1.year = 1 - exp(-rate.5.year*1)
rate.1.year = (-log(1-prob.1.year))/1 # this should be the exact same as rate.5.year, just checking 
annual.fraction.accepting.lai = prob.1.year 


# (B) Fraction who get virally suppressed
# per AFINAty poster: 
# "172 participants were enrolled across 3 cohorts, of which 134 were virally suppressed after screening and switched to LAI."
# don't have any differences by cohort 
#fraction.gaining.suppression = 134/172 # 0.7790698
fraction.gaining.suppression.es = 59/66 # 0.8939394
fraction.gaining.suppression.eu = 35/52 # 0.6730769
fraction.gaining.suppression.du = 40/54 # 0.7407407


# (C) Rate of suppression
# Different for the three cohorts 
# (original assumption: 3 months to gain suppression --> rate = 4)
# From engaged-suppressed: basically instantaneous? 1 week? rate = 52 
# From engaged-unsuppressed: 3 months to gain suppression --> rate = 4
# From diagnosed-unengaged: 1/(time to start ART (4/12, see below) + time to gain suppression (3/12)) = 12/7 = 1.714286

lai.suppression.rate.es = 52
lai.suppression.rate.eu = 4
lai.suppression.rate.du = 1.714286

if(1==2){
    # review baseline rate of suppression 
    sa.supp.prop = get.suppression.rates("South Africa")
    sa.supp.prop = sa.supp.prop$predictions
    sa.supp.prop = apply(sa.supp.prop,"year",mean) # these are proportions, converted to rates in map.model.parameters code 
    sa.supp.rates = -log(1-sa.supp.prop)
    cbind(sa.supp.rates) # rate of ~3 in modern era     

    # review baseline engagement - using a different way this time     
    x = get.engagement.model("South Africa")
    y = mean(x$data.array[4,,])
    -log(1-y) # 2017 rate of engagement is 2.3 
}

# ANNUAL RATE 
annual.lai.suppression.rate.es = annual.fraction.accepting.lai * fraction.gaining.suppression.es * lai.suppression.rate.es 
annual.lai.suppression.rate.eu = annual.fraction.accepting.lai * fraction.gaining.suppression.eu * lai.suppression.rate.eu 
annual.lai.suppression.rate.du = annual.fraction.accepting.lai * fraction.gaining.suppression.du * lai.suppression.rate.du 

# rapid - remove the fraction gaining? 
annual.lai.suppression.rate.eu.RAPID = annual.fraction.accepting.lai * fraction.gaining.suppression.eu * lai.suppression.rate.es # changed this last term
annual.lai.suppression.rate.du.RAPID = annual.fraction.accepting.lai * fraction.gaining.suppression.du * lai.suppression.rate.es  # changed this last term


