
source("helpers/pairing_helpers.R") # Todd wrote these for JHEEM 

# Bajos, 1995, Table 9: Age-mixing matrices (%) for heterosexual partnerships formed in the last 12 months 
# https://journals.lww.com/aidsonline/abstract/1995/07000/sexual_behaviour_and_hiv_epidemiology__comparative.11.aspx

# female to male is from the male's point of view (male ages; partner ages)
get.female.to.male.age.model.france = function(){
    
    dim.names = list(partner.age = c(16,19.5,23,29.5,39.5,52,70), # <=18, 18-21, 22-24, 25-34, 35-44, 45-59, >=60
                     age = c(19.5,23,29.5,39.5,52) # 18-21, 22-24, 25-34, 35-44, 45-59
    ) 
    data = c(.17,.72,.07,.04,0,0,0,
             .02,.37,.43,.18,0,0,0,
             .06,.09,.23,.58,.04,0,0,
             0,.04,.11,.37,.41,.07,0,
             0,.05,0,.21,.38,.36,0)
    data.array = array(data,
                       dim = sapply(dim.names,length),
                       dimnames = dim.names)
    data.array = aperm(data.array)
    data.array = rbind(rep(0,7),data.array,rep(0,7))
    
    age.cutoffs = c(16,18,22,25,35,45,60,75)
    age.counts = c(0,194,124,233,91,41,0)
    
    # fyi, this function generates random numbers; fine as long as I'm keeping the seed at the beginning of the MCMC 
    # assumes from is on the rows and to is on the columns
    expanded.data = matrix.to.scatter(t(data.array*age.counts),
                                      age.cutoffs = age.cutoffs) 
    
    age.model = fit.age.model(age.of.reference = expanded.data$age.to, # to is always the reference
                              age.of.partner = expanded.data$age.from) 
    
    rv = list(mean.age.diff.intercept = age.model["mean.intercept"],
              mean.age.diff.slope = age.model["mean.slope"],
              sd.age.diff.intercept = age.model["sd.intercept"],
              sd.age.diff.slope = age.model["sd.slope"])
    
    rv
    
}

# male to female is from the female's point of view (female ages; partner ages)
get.male.to.female.age.model.france = function(){
    
    dim.names = list(partner.age = c(16,19.5,23,29.5,39.5,52,70), # <=18, 18-21, 22-24, 25-34, 35-44, 45-59, >=60
                     age = c(19.5,23,29.5,39.5,52) # 18-21, 22-24, 25-34, 35-44, 45-59
    ) 
    data = c(0,.49,.29,.20,.02,0,0,
             0,.13,.36,.48,.03,0,0,
             0,.05,.12,.50,.30,.03,0,
             0,.07,.01,.39,.33,.20,0,
             0, 0, 0,.17,.13,.66,.04) # (to make it add to 1)
             # 0, 0, 0,.18,.14,.68,.04) # this doesn't add up
    data.array = array(data,
                       dim = sapply(dim.names,length),
                       dimnames = dim.names)
    data.array = aperm(data.array)
    data.array = rbind(rep(0,7),data.array,rep(0,7))
    
    age.cutoffs = c(16,18,22,25,35,45,60,75)
    age.counts = c(0,121,90,161,61,21,0)
    
    # fyi, this function generates random numbers; fine as long as I'm keeping the seed at the beginning of the MCMC 
    # assumes from is on the rows and to is on the columns
    expanded.data = matrix.to.scatter(t(data.array*age.counts),
                                      age.cutoffs = age.cutoffs) 
    
    age.model = fit.age.model(age.of.reference = expanded.data$age.to, # to is always the reference
                              age.of.partner = expanded.data$age.from) 
    
    rv = list(mean.age.diff.intercept = age.model["mean.intercept"],
              mean.age.diff.slope = age.model["mean.slope"],
              sd.age.diff.intercept = age.model["sd.intercept"],
              sd.age.diff.slope = age.model["sd.slope"])
    
    rv
}

# in the parameters browser, round(mixing.proportions.0[,1,,,2,],5)

