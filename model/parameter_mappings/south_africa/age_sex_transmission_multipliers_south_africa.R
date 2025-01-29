# SA DHS 2016, Tables 12.2.1 and 12.2.2 - high-risk sex = multiple partners 
# SA DHS 2003, Table 5.13 - high-risk sex = non-marital/non-cohabiting partner

get.transmission.multipliers.DHS.south.africa  = function(sex,
                                                          year){
    if(sex=="female"){
        if(year==2016){ # table 12.2.1
            data = data.frame(
                ages = c("15-19","20-24","25-29","30-39","40-49"),
                prop.high.risk.sex=c(.027,.066,.069,.049,.02), # 2+ partners in past 12 months 
                condom.use.among.high.risk.sex = c(.548,.640,.633,.515,.485)) 
        } else if(year==2008){
            
            
        } else if(year==2003){
            data = data.frame( # table 5.13
                ages = c("15-19","20-24","25-29","30-39","40-49"),
                prop.high.risk.sex=c(.948,.846,.642,.421,.338), # high risk sex in past 12 month
                condom.use.among.high.risk.sex = c(.492,.530,.455,.391,.380)) 
            
        }
    } else if(sex=="male"){
        if(year==2016){ # table 12.2.2
            data = data.frame(
                ages = c("15-19","20-24","25-29","30-39","40-49"),
                prop.high.risk.sex=c(.157,.262,.198,.170,.072), # 2+ partners in past 12 months 
                condom.use.among.high.risk.sex = c(.879,.630,.660,.555,.514))
        } else if(year==2008){
            
        } else if(year==2003){
            data = data.frame( # table 5.13
                ages = c("15-19","20-24","25-29","30-39","40-49"),
                prop.high.risk.sex=c(.991,.946,.836,.516,.252), # high risk sex in past 12 month
                condom.use.among.high.risk.sex = c(.250,.277,.305,.299,.408)) 
        }
    }
    
    data$prop.at.risk = data$prop.high.risk.sex*(1-data$condom.use.among.high.risk.sex)
    data$multiplier = data$prop.at.risk/(data$prop.at.risk[data$ages=="30-39"])
    
    rv = array(data$multiplier,
               dim = length(data$ages),
               dimnames = list(data$ages))
    
    rv
}


get.transmission.multipliers.older.cohort.south.africa = function(sex){
    if(sex=="female"){
        data = data.frame(
            ages = c("40-49","50-59","60-69","70-79"),
            prop.high.risk.sex=c(.015,.015,.003,.003), 
            # data says 50s and 70s have 0% multiple partners; that won't work, so instead, using 40s value for 50s and 60s value for 70s
            condom.use.among.high.risk.sex = c(.165,.102,0,0)) 
    } else if(sex=="male"){
        data = data.frame(
            ages = c("40-49","50-59","60-69","70-79"),
            prop.high.risk.sex=c(.195,.197,.123,.083),
            condom.use.among.high.risk.sex = c(.2,.06,.056,.018)) 
    }
    
    data$prop.at.risk = data$prop.high.risk.sex*(1-data$condom.use.among.high.risk.sex)
    data$multiplier = data$prop.at.risk/(data$prop.at.risk[data$ages=="40-49"])
    
    rv = array(data$multiplier,
               dim = length(data$ages),
               dimnames = list(data$ages))
    
    rv
    
}


get.all.transmission.multipliers.south.africa = function(sex,
                                                         year){
    multipliers = c(rep(0,length(get.age.brackets.in.range(lower = 0, upper = 15))),
                    rep(get.transmission.multipliers.DHS.south.africa(sex=sex, year=year)["15-19"],
                        length(get.age.brackets.in.range(lower = 15, upper = 20))),
                    rep(get.transmission.multipliers.DHS.south.africa(sex=sex, year=year)["20-24"],
                        length(get.age.brackets.in.range(lower = 20, upper = 25))),
                    rep(get.transmission.multipliers.DHS.south.africa(sex=sex, year=year)["25-29"],
                        length(get.age.brackets.in.range(lower = 25, upper = 30))),
                    rep(get.transmission.multipliers.DHS.south.africa(sex=sex, year=year)["30-39"],
                        length(get.age.brackets.in.range(lower = 30, upper = 40))),
                    rep(get.transmission.multipliers.DHS.south.africa(sex=sex, year=year)["40-49"],
                        length(get.age.brackets.in.range(lower = 40, upper = 50))),
                    
                    # multiply 40-49 from DHS by 50-59 multiplier from older cohort
                    rep((get.transmission.multipliers.DHS.south.africa(sex=sex, year=year)["40-49"]*
                             get.transmission.multipliers.older.cohort.south.africa(sex=sex)["50-59"]),
                        length(get.age.brackets.in.range(lower = 50, upper = 60))), 
                    
                    # multiply 40-49 from DHS by 60-69 multiplier from older cohort
                    rep((get.transmission.multipliers.DHS.south.africa(sex=sex, year=year)["40-49"]*
                             get.transmission.multipliers.older.cohort.south.africa(sex=sex)["60-69"]),
                        length(get.age.brackets.in.range(lower = 60, upper = 70))), 
                    
                    # multiply 40-49 from DHS by 70-79 multiplier from older cohort
                    rep((get.transmission.multipliers.DHS.south.africa(sex=sex, year=year)["40-49"]*
                             get.transmission.multipliers.older.cohort.south.africa(sex=sex)["70-79"]),
                        length(get.age.brackets.in.range(lower = 70, upper = Inf)))
    )
    names(multipliers) = get.age.brackets.in.range(lower=0,upper=Inf)
    multipliers
}



