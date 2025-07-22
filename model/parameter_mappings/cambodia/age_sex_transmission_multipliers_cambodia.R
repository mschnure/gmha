source("model/parameter_mappings/get_older_age_sex_transmission_multipliers.R")

# see DHS_tables.xlsx for what tables these came from 

# desired knot times: 2003, 2008, 2014
get.transmission.multipliers.DHS.cambodia  = function(sex,
                                                        year){
    if(sex=="female"){
        if(year==2022){ 
            data = data.frame(
                ages = c("15-19","20-24","25-29","30-39","40-49"),
                prop.high.risk.sex=c(.004,.002,.003,.003,.001), 
                condom.use.among.high.risk.sex = c(.002,.007,.011,(.016+.02)/2,(.022+.004)/2)) # missing for all ages; using table 7.3 instead (general condom use)  
        } else if(year==2014){
            data = data.frame(
                ages = c("15-19","20-24","25-29","30-39","40-49"),
                prop.high.risk.sex=c(.001,.003,.001,.001,.001), # 0's for 15-19, 30-39, 40-49 - swapped to 0.1 
                condom.use.among.high.risk.sex = c(.001,.011,.021,(.028+.021)/2,(.014+.008)/2)) # missing for all ages; using table 8.3 instead (general condom use)  
        } else if(year==2005){
            data = data.frame( 
                ages = c("15-19","20-24","25-29","30-39","40-49"),
                prop.high.risk.sex=c(.012,.040,.002,.001,.001),  # 0's for 30-39, 40-49 - swapped to 0.1 
                condom.use.among.high.risk.sex = c(.009,.046,.079,(.078+.069)/2,(.045+.035)/2)) # missing for all ages; using table 7.3 instead (general condom use)  
            
        }
    } else if(sex=="male"){
        if(year==2022){ 
            data = data.frame(
                ages = c("15-19","20-24","25-29","30-39","40-49"),
                prop.high.risk.sex=c(.026,.058,.067,.050,.026),
                condom.use.among.high.risk.sex = c(.722,.661,.711,.854,.718))
        } else if(year==2014){
            data = data.frame(
                ages = c("15-19","20-24","25-29","30-39","40-49"),
                prop.high.risk.sex=c(.002,.021,.029,.032,.043), 
                condom.use.among.high.risk.sex = c((.462*2)-.434,.434,(.434+.298)/2,.289,.188)) # 15-19 missing, but can get value from 15-24 and 20-24
                                                                                       # 25-29 missing; using average of 20-24 and 30-39 values
        } else if(year==2005){
            data = data.frame( 
                ages = c("15-19","20-24","25-29","30-39","40-49"),
                prop.high.risk.sex=c(.698,.309,.190,.086,.049), 
                condom.use.among.high.risk.sex = c(.802,.858,.861,.795,.748)) 
        }
    }
    
    data$prop.at.risk = data$prop.high.risk.sex*(1-data$condom.use.among.high.risk.sex)
    data$multiplier = data$prop.at.risk/(data$prop.at.risk[data$ages=="30-39"])
    
    rv = array(data$multiplier,
               dim = length(data$ages),
               dimnames = list(data$ages))
    
    rv
}

get.all.transmission.multipliers.cambodia = function(sex,
                                                       year){
    multipliers = c(rep(0,length(get.age.brackets.in.range(lower = 0, upper = 15))),
                    rep(get.transmission.multipliers.DHS.cambodia(sex=sex, year=year)["15-19"],
                        length(get.age.brackets.in.range(lower = 15, upper = 20))),
                    rep(get.transmission.multipliers.DHS.cambodia(sex=sex, year=year)["20-24"],
                        length(get.age.brackets.in.range(lower = 20, upper = 25))),
                    rep(get.transmission.multipliers.DHS.cambodia(sex=sex, year=year)["25-29"],
                        length(get.age.brackets.in.range(lower = 25, upper = 30))),
                    rep(get.transmission.multipliers.DHS.cambodia(sex=sex, year=year)["30-39"],
                        length(get.age.brackets.in.range(lower = 30, upper = 40))),
                    rep(get.transmission.multipliers.DHS.cambodia(sex=sex, year=year)["40-49"],
                        length(get.age.brackets.in.range(lower = 40, upper = 50))),
                    
                    # multiply 40-49 from DHS by 50-59 multiplier from older cohort
                    rep((get.transmission.multipliers.DHS.cambodia(sex=sex, year=year)["40-49"]*
                             get.transmission.multipliers.older.cohort.south.africa(sex=sex)["50-59"]),
                        length(get.age.brackets.in.range(lower = 50, upper = 60))), 
                    
                    # multiply 40-49 from DHS by 60-69 multiplier from older cohort
                    rep((get.transmission.multipliers.DHS.cambodia(sex=sex, year=year)["40-49"]*
                             get.transmission.multipliers.older.cohort.south.africa(sex=sex)["60-69"]),
                        length(get.age.brackets.in.range(lower = 60, upper = 70))), 
                    
                    # multiply 40-49 from DHS by 70-79 multiplier from older cohort
                    rep((get.transmission.multipliers.DHS.cambodia(sex=sex, year=year)["40-49"]*
                             get.transmission.multipliers.older.cohort.south.africa(sex=sex)["70-79"]),
                        length(get.age.brackets.in.range(lower = 70, upper = Inf)))
    )
    names(multipliers) = get.age.brackets.in.range(lower=0,upper=Inf)
    multipliers
}



