source("model/parameter_mappings/get_older_age_sex_transmission_multipliers.R")

# see DHS_tables.xlsx for what tables these came from 
get.transmission.multipliers.DHS.zimbabwe  = function(sex,
                                                    year){
    if(sex=="female"){
        if(year==2015){ 
            data = data.frame(
                ages = c("15-19","20-24","25-29","30-39","40-49"),
                prop.high.risk.sex=c(.008,.020,.012,.011,.005), 
                condom.use.among.high.risk.sex = c(.470,.638,.779,.712,.736)) 
        } else if(year==2010){
            data = data.frame(
                ages = c("15-19","20-24","25-29","30-39","40-49"),
                prop.high.risk.sex=c(.009,.017,.012,.007,.012), 
                condom.use.among.high.risk.sex = c(.407,.437,.541,.531,.423)) # missing values; used 2005 values instead
        } else if(year==2005){
            data = data.frame(
                ages = c("15-19","20-24","25-29","30-39","40-49"),
                prop.high.risk.sex=c(.028,.013,.012,.012,.007), 
                condom.use.among.high.risk.sex = c(.407,.437,.541,.531,.423)) 
            
        }
    } else if(sex=="male"){
        if(year==2015){ 
            data = data.frame(
                ages = c("15-19","20-24","25-29","30-39","40-49"),
                prop.high.risk.sex=c(.039,.180,.200,.190,.147),
                condom.use.among.high.risk.sex = c(.788,.875,.865,.866,.870))
        } else if(year==2010){
            data = data.frame(
                ages = c("15-19","20-24","25-29","30-39","40-49"),
                prop.high.risk.sex=c(.025,.142,.144,.138,.089), 
                condom.use.among.high.risk.sex = c(.662,.470,.341,.193,.232)) 
        } else if(year==2005){
            data = data.frame(
                ages = c("15-19","20-24","25-29","30-39","40-49"),
                prop.high.risk.sex=c(.152,.217,.159,.106,.098),
                condom.use.among.high.risk.sex = c(.544,.756,.779,.799,.559))
        }
    }
    
    data$prop.at.risk = data$prop.high.risk.sex*(1-data$condom.use.among.high.risk.sex)
    data$multiplier = data$prop.at.risk/(data$prop.at.risk[data$ages=="30-39"])
    
    rv = array(data$multiplier,
               dim = length(data$ages),
               dimnames = list(data$ages))
    
    rv
}

get.all.transmission.multipliers.zimbabwe = function(sex,
                                                   year){
    multipliers = c(rep(0,length(get.age.brackets.in.range(lower = 0, upper = 15))),
                    rep(get.transmission.multipliers.DHS.zimbabwe(sex=sex, year=year)["15-19"],
                        length(get.age.brackets.in.range(lower = 15, upper = 20))),
                    rep(get.transmission.multipliers.DHS.zimbabwe(sex=sex, year=year)["20-24"],
                        length(get.age.brackets.in.range(lower = 20, upper = 25))),
                    rep(get.transmission.multipliers.DHS.zimbabwe(sex=sex, year=year)["25-29"],
                        length(get.age.brackets.in.range(lower = 25, upper = 30))),
                    rep(get.transmission.multipliers.DHS.zimbabwe(sex=sex, year=year)["30-39"],
                        length(get.age.brackets.in.range(lower = 30, upper = 40))),
                    rep(get.transmission.multipliers.DHS.zimbabwe(sex=sex, year=year)["40-49"],
                        length(get.age.brackets.in.range(lower = 40, upper = 50))),
                    
                    # multiply 40-49 from DHS by 50-59 multiplier from older cohort
                    rep((get.transmission.multipliers.DHS.zimbabwe(sex=sex, year=year)["40-49"]*
                             get.transmission.multipliers.older.cohort.south.africa(sex=sex)["50-59"]),
                        length(get.age.brackets.in.range(lower = 50, upper = 60))), 
                    
                    # multiply 40-49 from DHS by 60-69 multiplier from older cohort
                    rep((get.transmission.multipliers.DHS.zimbabwe(sex=sex, year=year)["40-49"]*
                             get.transmission.multipliers.older.cohort.south.africa(sex=sex)["60-69"]),
                        length(get.age.brackets.in.range(lower = 60, upper = 70))), 
                    
                    # multiply 40-49 from DHS by 70-79 multiplier from older cohort
                    rep((get.transmission.multipliers.DHS.zimbabwe(sex=sex, year=year)["40-49"]*
                             get.transmission.multipliers.older.cohort.south.africa(sex=sex)["70-79"]),
                        length(get.age.brackets.in.range(lower = 70, upper = Inf)))
    )
    names(multipliers) = get.age.brackets.in.range(lower=0,upper=Inf)
    multipliers
}



