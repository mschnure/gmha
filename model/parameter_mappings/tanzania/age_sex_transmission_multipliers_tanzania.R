source("model/parameter_mappings/get_older_age_sex_transmission_multipliers.R")

# see DHS_tables.xlsx for what tables these came from 

get.transmission.multipliers.DHS.tanzania  = function(sex,
                                                        year){
    if(sex=="female"){
        if(year==2022){ 
            data = data.frame(
                ages = c("15-19","20-24","25-29","30-39","40-49"),
                prop.high.risk.sex=c(.030,.050,.058,.047,.034), 
                condom.use.among.high.risk.sex = c(.188,.111,.119,.164,.088)) 
        } else if(year==2010){
            data = data.frame(
                ages = c("15-19","20-24","25-29","30-39","40-49"),
                prop.high.risk.sex=c(.019,.047,.046,.037,.028), 
                condom.use.among.high.risk.sex = c(.353,.299,.321,.226,.166)) 
        } else if(year==2004){
            data = data.frame( 
                ages = c("15-19","20-24","25-29","30-39","40-49"),
                prop.high.risk.sex=c(.048,.050,.043,.040,.032), 
                condom.use.among.high.risk.sex = c(.364,.311,.296,.207,.130)) 
            
        }
    } else if(sex=="male"){
        if(year==2022){ 
            data = data.frame(
                ages = c("15-19","20-24","25-29","30-39","40-49"),
                prop.high.risk.sex=c(.070,.286,.349,.259,.267),
                condom.use.among.high.risk.sex = c(.482,.297,.278,.147,.144))
        } else if(year==2010){
            data = data.frame(
                ages = c("15-19","20-24","25-29","30-39","40-49"),
                prop.high.risk.sex=c(.068,.209,.300,.241,.278), 
                condom.use.among.high.risk.sex = c(.342,.373,.258,.235,.097)) 
        } else if(year==2004){
            data = data.frame( 
                ages = c("15-19","20-24","25-29","30-39","40-49"),
                prop.high.risk.sex=c(.265,.371,.330,.297,.236), 
                condom.use.among.high.risk.sex = c(.390,.505,.582,.574,.566)) 
        }
    }
    
    data$prop.at.risk = data$prop.high.risk.sex*(1-data$condom.use.among.high.risk.sex)
    data$multiplier = data$prop.at.risk/(data$prop.at.risk[data$ages=="30-39"])
    
    rv = array(data$multiplier,
               dim = length(data$ages),
               dimnames = list(data$ages))
    
    rv
}

get.all.transmission.multipliers.tanzania = function(sex,
                                                         year){
    multipliers = c(rep(0,length(get.age.brackets.in.range(lower = 0, upper = 15))),
                    rep(get.transmission.multipliers.DHS.tanzania(sex=sex, year=year)["15-19"],
                        length(get.age.brackets.in.range(lower = 15, upper = 20))),
                    rep(get.transmission.multipliers.DHS.tanzania(sex=sex, year=year)["20-24"],
                        length(get.age.brackets.in.range(lower = 20, upper = 25))),
                    rep(get.transmission.multipliers.DHS.tanzania(sex=sex, year=year)["25-29"],
                        length(get.age.brackets.in.range(lower = 25, upper = 30))),
                    rep(get.transmission.multipliers.DHS.tanzania(sex=sex, year=year)["30-39"],
                        length(get.age.brackets.in.range(lower = 30, upper = 40))),
                    rep(get.transmission.multipliers.DHS.tanzania(sex=sex, year=year)["40-49"],
                        length(get.age.brackets.in.range(lower = 40, upper = 50))),
                    
                    # multiply 40-49 from DHS by 50-59 multiplier from older cohort
                    rep((get.transmission.multipliers.DHS.tanzania(sex=sex, year=year)["40-49"]*
                             get.transmission.multipliers.older.cohort.south.africa(sex=sex)["50-59"]),
                        length(get.age.brackets.in.range(lower = 50, upper = 60))), 
                    
                    # multiply 40-49 from DHS by 60-69 multiplier from older cohort
                    rep((get.transmission.multipliers.DHS.tanzania(sex=sex, year=year)["40-49"]*
                             get.transmission.multipliers.older.cohort.south.africa(sex=sex)["60-69"]),
                        length(get.age.brackets.in.range(lower = 60, upper = 70))), 
                    
                    # multiply 40-49 from DHS by 70-79 multiplier from older cohort
                    rep((get.transmission.multipliers.DHS.tanzania(sex=sex, year=year)["40-49"]*
                             get.transmission.multipliers.older.cohort.south.africa(sex=sex)["70-79"]),
                        length(get.age.brackets.in.range(lower = 70, upper = Inf)))
    )
    names(multipliers) = get.age.brackets.in.range(lower=0,upper=Inf)
    multipliers
}



