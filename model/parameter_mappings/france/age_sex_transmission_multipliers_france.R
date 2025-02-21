
# Comportements Sexuels en France (CSF) --> Sexual Behavior in France

# years I want: 2003, 2008, 2014
# France CSF survey years: 
    # 1992 --> 2003? ACTUALLY, just using 2007 for this too 
    # 2007 --> 2008 
    # 2023 --> 2014 

# 1992 CSF 
get.transmission.multipliers.france.1992 = function(sex){
    if(sex=="female"){
        data = data.frame(
            ages = c("18-24","25-34","35-44","45-69"),
            prop.high.risk.sex=c(.121,.068,.059,.029), # multipartner - 1 year
            condom.use.among.high.risk.sex = c(.624,.620,.333,.331)) # condom use among multipartner
    } else if(sex=="male"){
        data = data.frame(
            ages = c("18-24","25-34","35-44","45-69"),
            prop.high.risk.sex=c(.276,.141,.115,.083), # multipartner - 1 year
            condom.use.among.high.risk.sex = c(.790,.690,.597,.427)) # condom use among multipartner
    }
    
    data$prop.at.risk = data$prop.high.risk.sex*(1-data$condom.use.among.high.risk.sex)
    data$multiplier = data$prop.at.risk/(data$prop.at.risk[data$ages=="35-44"])
    
    rv = array(data$multiplier,
               dim = length(data$ages),
               dimnames = list(data$ages))
    
    rv
    
}

# 2007 CSF
# multiple partners I am estimating from a figure! (see CSF_2007_figure.pptx)
get.transmission.multipliers.france.2007 = function(sex){
    if(sex=="female"){
        data = data.frame(
            ages = c("18-19","20-24","25-34","35-39","40-49","50-59","60-69"),
            prop.high.risk.sex=c(.223,.195,.096,.072,.054,.019,.012)) # multipartner - 1 year
            # don't have condom use by age 
    } else if(sex=="male"){
        data = data.frame(
            ages = c("18-19","20-24","25-34","35-39","40-49","50-59","60-69"),
            prop.high.risk.sex=c(.276,.317,.170,.09,.072,.08,.041)) # multipartner - 1 year
            # don't have condom use by age 
    }
    
    data$prop.at.risk = data$prop.high.risk.sex
    data$multiplier = data$prop.at.risk/(data$prop.at.risk[data$ages=="35-39"])
    
    rv = array(data$multiplier,
               dim = length(data$ages),
               dimnames = list(data$ages))
    
    rv
    
}

# 2023 CSF
# multiple partners in past year not reported by age 
get.transmission.multipliers.france.2023 = function(sex){
    if(sex=="female"){
        data = data.frame(
            ages = c("18-29","30-39","40-49","50-59","60-69","70-89"),
            # don't have multiple partnerships by age, but need some estimate of high-risk sex, so using meeting a sexual partner online instead
            prop.high.risk.sex = c(.394,.320,.194,.106,.06,.018), # met partner online 
            condom.use.with.new.partner = c(.505,.561,.460,.50,.50,.50),
            sex.past.year=c(1,1,1,1,(.608/.758),(((.428+.109)/2)/.758)))
            # data says 60s and 70s have 0% condom use but that's because only 19 women with new partner;
            # that won't work, so instead, taking sexual intercourse in the last year for 60-69 and 70-79/80-89 relative to 50-59
            
        data$condom.use = data$condom.use.with.new.partner*data$sex.past.year
        
    } else if(sex=="male"){
        data = data.frame(
            ages = c("18-29","30-39","40-49","50-59","60-69","70-89"),
            # don't have multiple partnerships by age, but need some estimate of high-risk sex, so using meeting a sexual partner online instead
            prop.high.risk.sex = c(.435,.393,.268,.168,.077,.041), # met partner online 
            condom.use.with.new.partner = c(.602,.547,.494,.443,.266,.214))
            
    }
    
    data$prop.at.risk = data$prop.high.risk.sex*(1-data$condom.use)
    data$multiplier = data$prop.at.risk/(data$prop.at.risk[data$ages=="30-39"])
    
    rv = array(data$multiplier,
               dim = length(data$ages),
               dimnames = list(data$ages))
    
    rv
    
}

get.all.transmission.multipliers.france = function(sex,
                                                  year){
    if(year==1992){
        multipliers = c(rep(0,length(get.age.brackets.in.range(lower = 0, upper = 15))),
                        rep(get.transmission.multipliers.france.1992(sex=sex)["18-24"],
                            length(get.age.brackets.in.range(lower = 15, upper = 25))),
                        rep(get.transmission.multipliers.france.1992(sex=sex)["25-34"],
                            length(get.age.brackets.in.range(lower = 25, upper = 35))),
                        rep(get.transmission.multipliers.france.1992(sex=sex)["35-44"],
                            length(get.age.brackets.in.range(lower = 35, upper = 45))),
                        rep(get.transmission.multipliers.france.1992(sex=sex)["45-69"],
                            length(get.age.brackets.in.range(lower = 45, upper = Inf)))
        )
    } else if(year==2007){
        multipliers = c(rep(0,length(get.age.brackets.in.range(lower = 0, upper = 15))),
                        rep(get.transmission.multipliers.france.2007(sex=sex)["18-19"],
                            length(get.age.brackets.in.range(lower = 15, upper = 20))),
                        rep(get.transmission.multipliers.france.2007(sex=sex)["20-24"],
                            length(get.age.brackets.in.range(lower = 20, upper = 25))),
                        rep(get.transmission.multipliers.france.2007(sex=sex)["25-34"],
                            length(get.age.brackets.in.range(lower = 25, upper = 35))),
                        rep(get.transmission.multipliers.france.2007(sex=sex)["35-39"],
                            length(get.age.brackets.in.range(lower = 35, upper = 40))),
                        rep(get.transmission.multipliers.france.2007(sex=sex)["40-49"],
                            length(get.age.brackets.in.range(lower = 40, upper = 50))),
                        rep(get.transmission.multipliers.france.2007(sex=sex)["50-59"],
                            length(get.age.brackets.in.range(lower = 50, upper = 60))),
                        rep(get.transmission.multipliers.france.2007(sex=sex)["60-69"],
                            length(get.age.brackets.in.range(lower = 60, upper = Inf)))
        )
    } else if(year==2023){
        multipliers = c(rep(0,length(get.age.brackets.in.range(lower = 0, upper = 15))),
                        rep(get.transmission.multipliers.france.2023(sex=sex)["18-29"],
                            length(get.age.brackets.in.range(lower = 15, upper = 30))),
                        rep(get.transmission.multipliers.france.2023(sex=sex)["30-39"],
                            length(get.age.brackets.in.range(lower = 30, upper = 40))),
                        rep(get.transmission.multipliers.france.2023(sex=sex)["40-49"],
                            length(get.age.brackets.in.range(lower = 40, upper = 50))),
                        rep(get.transmission.multipliers.france.2023(sex=sex)["50-59"],
                            length(get.age.brackets.in.range(lower = 50, upper = 60))),
                        rep(get.transmission.multipliers.france.2023(sex=sex)["60-69"],
                            length(get.age.brackets.in.range(lower = 60, upper = 70))),
                        rep(get.transmission.multipliers.france.2023(sex=sex)["70-89"],
                            length(get.age.brackets.in.range(lower = 70, upper = Inf)))
        )
    } else stop("can only pull years 1992, 2007, and 2023")
    
    names(multipliers) = get.age.brackets.in.range(lower=0,upper=Inf)
    multipliers
}


