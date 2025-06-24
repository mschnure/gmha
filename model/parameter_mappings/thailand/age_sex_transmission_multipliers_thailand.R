

# desired knot times: 2003, 2008, 2014
# data times: 2006, 2012, 2014
# 2006 estimate is from Ford et al 2009, which cites data from 2006 National Sexual Behavior Survey (can't find actual underlying data)
# 2012 estimate is from Techasrivichien et al, 2014, which is a survey of adults in a province near Bangkok
# 2014 estimate is from Pinyopornpanish et al, 2017, which is a survey of adults in Chiang Mai 
get.transmission.multipliers.thailand.2014  = function(sex){
    if(sex=="female"){
        data = data.frame(
            ages = c("15-19","20-29","30-39","40-49","50-59","60+"), 
            prop.high.risk.sex=c(.0412,.0093,.0079,.0026,.0037,.001), # prop with >1 sexual partner in past 3 months; for missing female data, use total
            condom.use.among.high.risk.sex = c(.172,.048,.070,.021,.022,.014)) # percent condom use in past 3 months 
    } else if(sex=="male"){
        data = data.frame(
            ages = c("15-19","20-29","30-39","40-49","50-59","60+"), 
            prop.high.risk.sex=c(.0801,.0159,.0153,.0062,.0037,.001), # prop with >1 sexual partner in past 3 months 
            condom.use.among.high.risk.sex = c(.237,.185,.071,.035,.034,.028)) 
    }
    
    
    data$prop.at.risk = data$prop.high.risk.sex*(1-data$condom.use.among.high.risk.sex)
    data$multiplier = data$prop.at.risk/(data$prop.at.risk[data$ages=="30-39"])
    
    rv = array(data$multiplier,
               dim = length(data$ages),
               dimnames = list(data$ages))
    
    rv
}

get.transmission.multipliers.thailand.2012  = function(sex){
    if(sex=="female"){
        data = data.frame(
            ages = c("15-19","20-24","25-34","35-44","45-54","55-59"), 
            prop.high.risk.sex=c(0.09,0.032,0.025,0.009,0.001,0.001), # in past 12 months, had casual sex 
                                                                    # made 45-54 and 55-59 0.001 instead of 0
            condom.use.among.high.risk.sex = c(0,0,0,0,0,0)) # did not report condom use 
    } else if(sex=="male"){
        data = data.frame(
            ages = c("15-19","20-24","25-34","35-44","45-54","55-59"), 
            prop.high.risk.sex=c(.316,.297,.213,.093,.072,.021), # in past 12 months, had casual sex 
            condom.use.among.high.risk.sex = c(0,0,0,0,0,0)) # did not report condom use
    }
    
    
    data$prop.at.risk = data$prop.high.risk.sex*(1-data$condom.use.among.high.risk.sex)
    data$multiplier = data$prop.at.risk/(data$prop.at.risk[data$ages=="35-44"])
    
    rv = array(data$multiplier,
               dim = length(data$ages),
               dimnames = list(data$ages))
    
    rv
}

get.transmission.multipliers.thailand.2006  = function(sex){
    if(sex=="female"){
        data = data.frame(
            ages = c("18-39","40-49","50-59"), 
            prop.high.risk.sex=c(0.6,0.2,0.1), # had sex with a casual partner 
            # (made 50-59 0.1 instead of 0)
            condom.use.among.high.risk.sex = c(.426,.286,.142)) #  always use condom with casual partner
        # have to use male percentages
    } else if(sex=="male"){
        data = data.frame(
            ages = c("18-39","40-49","50-59"),
            prop.high.risk.sex=c(.176,.044,.021), # had sex with a casual partner 
            condom.use.among.high.risk.sex = c(.426,.286,.142)) #  always use condom with casual partner
    }
    
    data$prop.at.risk = data$prop.high.risk.sex*(1-data$condom.use.among.high.risk.sex)
    data$multiplier = data$prop.at.risk/(data$prop.at.risk[data$ages=="40-49"])
    
    rv = array(data$multiplier,
               dim = length(data$ages),
               dimnames = list(data$ages))
    
    rv
}

get.all.transmission.multipliers.thailand = function(sex,
                                                     year){
    if(year==2006){
        multipliers = c(rep(0,length(get.age.brackets.in.range(lower = 0, upper = 15))),
                        rep(get.transmission.multipliers.thailand.2006(sex=sex)["18-39"],
                            length(get.age.brackets.in.range(lower = 15, upper = 40))),
                        rep(get.transmission.multipliers.thailand.2006(sex=sex)["40-49"],
                            length(get.age.brackets.in.range(lower = 40, upper = 50))),
                        rep(get.transmission.multipliers.thailand.2006(sex=sex)["50-59"],
                            length(get.age.brackets.in.range(lower = 50, upper = Inf)))
        )
    } else if(year==2012){
        multipliers = c(rep(0,length(get.age.brackets.in.range(lower = 0, upper = 15))),
                        rep(get.transmission.multipliers.thailand.2012(sex=sex)["15-19"],
                            length(get.age.brackets.in.range(lower = 15, upper = 20))),
                        rep(get.transmission.multipliers.thailand.2012(sex=sex)["20-24"],
                            length(get.age.brackets.in.range(lower = 20, upper = 25))),
                        rep(get.transmission.multipliers.thailand.2012(sex=sex)["25-34"],
                            length(get.age.brackets.in.range(lower = 25, upper = 35))),
                        rep(get.transmission.multipliers.thailand.2012(sex=sex)["35-44"],
                            length(get.age.brackets.in.range(lower = 35, upper = 45))),
                        rep(get.transmission.multipliers.thailand.2012(sex=sex)["45-54"],
                            length(get.age.brackets.in.range(lower = 45, upper = 55))),
                        rep(get.transmission.multipliers.thailand.2012(sex=sex)["55-59"],
                            length(get.age.brackets.in.range(lower = 55, upper = Inf)))
        )
    } else if(year==2014){ #   ages = c("15-19","20-29","30-39","40-49","50-59","60+"), 
        multipliers = c(rep(0,length(get.age.brackets.in.range(lower = 0, upper = 15))),
                        rep(get.transmission.multipliers.thailand.2014(sex=sex)["15-19"],
                            length(get.age.brackets.in.range(lower = 15, upper = 20))),
                        rep(get.transmission.multipliers.thailand.2014(sex=sex)["20-29"],
                            length(get.age.brackets.in.range(lower = 20, upper = 30))),
                        rep(get.transmission.multipliers.thailand.2014(sex=sex)["30-39"],
                            length(get.age.brackets.in.range(lower = 30, upper = 40))),
                        rep(get.transmission.multipliers.thailand.2014(sex=sex)["40-49"],
                            length(get.age.brackets.in.range(lower = 40, upper = 50))),
                        rep(get.transmission.multipliers.thailand.2014(sex=sex)["50-59"],
                            length(get.age.brackets.in.range(lower = 50, upper = 60))),
                        rep(get.transmission.multipliers.thailand.2014(sex=sex)["60+"],
                            length(get.age.brackets.in.range(lower = 60, upper = Inf)))
        )
    } else stop("can only pull years 2006, 2012, and 2014")
    
    names(multipliers) = get.age.brackets.in.range(lower=0,upper=Inf)
    multipliers
}