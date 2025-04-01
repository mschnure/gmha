source("model/parameter_mappings/get_older_age_sex_transmission_multipliers.R")

# see DHS_tables.xlsx for what tables these came from 
get.transmission.multipliers.DHS.nigeria  = function(sex,
                                                    year){
    if(sex=="female"){
        if(year==2013){ 
            data = data.frame(
                ages = c("15-19","20-24","25-29","30-39","40-49"),
                prop.high.risk.sex=c(.007,.016,.012,.010,.011), 
                condom.use.among.high.risk.sex = c(.381,.419,.361,.228,.071)) 
        } else if(year==2008){
            data = data.frame(
                ages = c("15-19","20-24","25-29","30-39","40-49"),
                prop.high.risk.sex=c(.010,.015,.014,.008,.006), 
                condom.use.among.high.risk.sex = c(.286,.405,.384,.257,.045)) 
        } else if(year==2003){
            data = data.frame(
                ages = c("15-19","20-24","25-29","30-39","40-49"),
                prop.high.risk.sex=c(.346,.258,.106,.048,.040), 
                condom.use.among.high.risk.sex = c(.220,.260,.293,.149,.083)) 
            
        }
    } else if(sex=="male"){
        if(year==2013){ 
            data = data.frame(
                ages = c("15-19","20-24","25-29","30-39","40-49"),
                prop.high.risk.sex=c(.011,.075,.130,.173,.232),
                condom.use.among.high.risk.sex = c(.461,.513,.389,.171,.044))
        } else if(year==2008){
            data = data.frame(
                ages = c("15-19","20-24","25-29","30-39","40-49"),
                prop.high.risk.sex=c(.024,.094,.125,.117,.126), 
                condom.use.among.high.risk.sex = c(.363,.551,.607,.585,.465)) 
        } else if(year==2003){
            data = data.frame(
                ages = c("15-19","20-24","25-29","30-39","40-49"),
                prop.high.risk.sex=c(.941,.724,.557,.233,.144),
                condom.use.among.high.risk.sex = c(.328,.530,.438,.480,.569))
        }
    }
    
    data$prop.at.risk = data$prop.high.risk.sex*(1-data$condom.use.among.high.risk.sex)
    data$multiplier = data$prop.at.risk/(data$prop.at.risk[data$ages=="30-39"])
    
    rv = array(data$multiplier,
               dim = length(data$ages),
               dimnames = list(data$ages))
    
    rv
}

get.all.transmission.multipliers.nigeria = function(sex,
                                                   year){
    multipliers = c(rep(0,length(get.age.brackets.in.range(lower = 0, upper = 15))),
                    rep(get.transmission.multipliers.DHS.nigeria(sex=sex, year=year)["15-19"],
                        length(get.age.brackets.in.range(lower = 15, upper = 20))),
                    rep(get.transmission.multipliers.DHS.nigeria(sex=sex, year=year)["20-24"],
                        length(get.age.brackets.in.range(lower = 20, upper = 25))),
                    rep(get.transmission.multipliers.DHS.nigeria(sex=sex, year=year)["25-29"],
                        length(get.age.brackets.in.range(lower = 25, upper = 30))),
                    rep(get.transmission.multipliers.DHS.nigeria(sex=sex, year=year)["30-39"],
                        length(get.age.brackets.in.range(lower = 30, upper = 40))),
                    rep(get.transmission.multipliers.DHS.nigeria(sex=sex, year=year)["40-49"],
                        length(get.age.brackets.in.range(lower = 40, upper = 50))),
                    
                    # multiply 40-49 from DHS by 50-59 multiplier from older cohort
                    rep((get.transmission.multipliers.DHS.nigeria(sex=sex, year=year)["40-49"]*
                             get.transmission.multipliers.older.cohort.south.africa(sex=sex)["50-59"]),
                        length(get.age.brackets.in.range(lower = 50, upper = 60))), 
                    
                    # multiply 40-49 from DHS by 60-69 multiplier from older cohort
                    rep((get.transmission.multipliers.DHS.nigeria(sex=sex, year=year)["40-49"]*
                             get.transmission.multipliers.older.cohort.south.africa(sex=sex)["60-69"]),
                        length(get.age.brackets.in.range(lower = 60, upper = 70))), 
                    
                    # multiply 40-49 from DHS by 70-79 multiplier from older cohort
                    rep((get.transmission.multipliers.DHS.nigeria(sex=sex, year=year)["40-49"]*
                             get.transmission.multipliers.older.cohort.south.africa(sex=sex)["70-79"]),
                        length(get.age.brackets.in.range(lower = 70, upper = Inf)))
    )
    names(multipliers) = get.age.brackets.in.range(lower=0,upper=Inf)
    multipliers
}



