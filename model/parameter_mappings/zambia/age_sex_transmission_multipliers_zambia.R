source("model/parameter_mappings/get_older_age_sex_transmission_multipliers.R")

# see DHS_tables.xlsx for what tables these came from 
get.transmission.multipliers.DHS.zambia  = function(sex,
                                                    year){
    if(sex=="female"){
        if(year==2018){ 
            data = data.frame(
                ages = c("15-19","20-24","25-29","30-39","40-49"),
                prop.high.risk.sex=c(.009,.024,.016,.017,.008), 
                condom.use.among.high.risk.sex = c(.296,.381,.362,.343,.375)) 
        } else if(year==2007){
            data = data.frame(
                ages = c("15-19","20-24","25-29","30-39","40-49"),
                prop.high.risk.sex=c(.019,.010,.011,.012,.006), 
                condom.use.among.high.risk.sex = c(.360,.408,.444,.328,.271)) 
        } else if(year==2001){
            data = data.frame(
                ages = c("15-19","20-24","25-29","30-39","40-49"),
                prop.high.risk.sex=c(.029,.021,.032,.020,.022), 
                condom.use.among.high.risk.sex = c(.302,.382,.406,.332,.165)) 
            
        }
    } else if(sex=="male"){
        if(year==2018){ 
            data = data.frame(
                ages = c("15-19","20-24","25-29","30-39","40-49"),
                prop.high.risk.sex=c(.059,.169,.215,.189,.169),
                condom.use.among.high.risk.sex = c(.456,.519,.628,.609,.466))
        } else if(year==2007){
            data = data.frame(
                ages = c("15-19","20-24","25-29","30-39","40-49"),
                prop.high.risk.sex=c(.045,.144,.201,.190,.152), 
                condom.use.among.high.risk.sex = c(.421,.523,.550,.511,.494)) 
        } else if(year==2001){
            data = data.frame(
                ages = c("15-19","20-24","25-29","30-39","40-49"),
                prop.high.risk.sex=c(.119,.244,.248,.176,.141),
                condom.use.among.high.risk.sex = c(.325,.519,.467,.493,.401))
        }
    }
    
    data$prop.at.risk = data$prop.high.risk.sex*(1-data$condom.use.among.high.risk.sex)
    data$multiplier = data$prop.at.risk/(data$prop.at.risk[data$ages=="30-39"])
    
    rv = array(data$multiplier,
               dim = length(data$ages),
               dimnames = list(data$ages))
    
    rv
}

get.all.transmission.multipliers.zambia = function(sex,
                                                   year){
    multipliers = c(rep(0,length(get.age.brackets.in.range(lower = 0, upper = 15))),
                    rep(get.transmission.multipliers.DHS.zambia(sex=sex, year=year)["15-19"],
                        length(get.age.brackets.in.range(lower = 15, upper = 20))),
                    rep(get.transmission.multipliers.DHS.zambia(sex=sex, year=year)["20-24"],
                        length(get.age.brackets.in.range(lower = 20, upper = 25))),
                    rep(get.transmission.multipliers.DHS.zambia(sex=sex, year=year)["25-29"],
                        length(get.age.brackets.in.range(lower = 25, upper = 30))),
                    rep(get.transmission.multipliers.DHS.zambia(sex=sex, year=year)["30-39"],
                        length(get.age.brackets.in.range(lower = 30, upper = 40))),
                    rep(get.transmission.multipliers.DHS.zambia(sex=sex, year=year)["40-49"],
                        length(get.age.brackets.in.range(lower = 40, upper = 50))),
                    
                    # multiply 40-49 from DHS by 50-59 multiplier from older cohort
                    rep((get.transmission.multipliers.DHS.zambia(sex=sex, year=year)["40-49"]*
                             get.transmission.multipliers.older.cohort.south.africa(sex=sex)["50-59"]),
                        length(get.age.brackets.in.range(lower = 50, upper = 60))), 
                    
                    # multiply 40-49 from DHS by 60-69 multiplier from older cohort
                    rep((get.transmission.multipliers.DHS.zambia(sex=sex, year=year)["40-49"]*
                             get.transmission.multipliers.older.cohort.south.africa(sex=sex)["60-69"]),
                        length(get.age.brackets.in.range(lower = 60, upper = 70))), 
                    
                    # multiply 40-49 from DHS by 70-79 multiplier from older cohort
                    rep((get.transmission.multipliers.DHS.zambia(sex=sex, year=year)["40-49"]*
                             get.transmission.multipliers.older.cohort.south.africa(sex=sex)["70-79"]),
                        length(get.age.brackets.in.range(lower = 70, upper = Inf)))
    )
    names(multipliers) = get.age.brackets.in.range(lower=0,upper=Inf)
    multipliers
}



