source("model/parameter_mappings/get_older_age_sex_transmission_multipliers.R")

# KDHS 2014, Tables 13.9.1 and 13.9.2 - high-risk sex = multiple partners 
# KDHS 2008, Tables  13.7.1 and 13.7.2 - high-risk sex = non-marital/non-cohabiting partner
# KDHS 2003, Table 12.12 - high-risk sex = non-marital/non-cohabiting partner
get.transmission.multipliers.DHS.kenya  = function(sex,
                                                   year){
    if(sex=="female"){
        if(year==2014){
            data = data.frame(
                ages = c("15-19","20-24","25-29","30-39","40-49"),
                prop.high.risk.sex=c(.01,.02,.013,.016,.009), # 2+ partners
                condom.use.among.high.risk.sex = c(.261,.434,.431,.480,.480)) 
        } else if(year==2008){
            data = data.frame(
                ages = c("15-19","20-24","25-29","30-39","40-49"),
                prop.high.risk.sex=c(.154,.181,.125,.092,.087),
                condom.use.among.high.risk.sex = c(.41,.382,.318,.312,.264)) 
        } else if(year==2003){
            data = data.frame(
                ages = c("15-19","20-24","25-29","30-39","40-49"),
                prop.high.risk.sex=c(.467,.214,.139,.108,.109),
                condom.use.among.high.risk.sex = c(.234,.276,.258,.231,.149)) 
        }
    } else if(sex=="male"){
        if(year==2014){
            data = data.frame(
                ages = c("15-19","20-24","25-29","30-39","40-49"),
                prop.high.risk.sex=c(.037,.167,.173,.146,.119),
                condom.use.among.high.risk.sex = c(.641,.702,.501,.282,.222))
        } else if(year==2008){
            data = data.frame(
                ages = c("15-19","20-24","25-29","30-39","40-49"),
                prop.high.risk.sex=c(.243,.493,.31,.167,.072),
                condom.use.among.high.risk.sex = c(.547,.701,.654,.548,.474)) 
        } else if(year==2003){
            data = data.frame(
                ages = c("15-19","20-24","25-29","30-39","40-49"),
                prop.high.risk.sex=c(.971,.772,.353,.186,.096),
                condom.use.among.high.risk.sex = c(.413,.507,.518,.386,.5)) 
        }
    }
    
    data$prop.at.risk = data$prop.high.risk.sex*(1-data$condom.use.among.high.risk.sex)
    data$multiplier = data$prop.at.risk/(data$prop.at.risk[data$ages=="30-39"])
    
    rv = array(data$multiplier,
               dim = length(data$ages),
               dimnames = list(data$ages))
    
    rv
}

get.all.transmission.multipliers.kenya = function(sex,
                                                  year){
    multipliers = c(rep(0,length(get.age.brackets.in.range(lower = 0, upper = 15))),
                    rep(get.transmission.multipliers.DHS.kenya(sex=sex, year=year)["15-19"],
                        length(get.age.brackets.in.range(lower = 15, upper = 20))),
                    rep(get.transmission.multipliers.DHS.kenya(sex=sex, year=year)["20-24"],
                        length(get.age.brackets.in.range(lower = 20, upper = 25))),
                    rep(get.transmission.multipliers.DHS.kenya(sex=sex, year=year)["25-29"],
                        length(get.age.brackets.in.range(lower = 25, upper = 30))),
                    rep(get.transmission.multipliers.DHS.kenya(sex=sex, year=year)["30-39"],
                        length(get.age.brackets.in.range(lower = 30, upper = 40))),
                    rep(get.transmission.multipliers.DHS.kenya(sex=sex, year=year)["40-49"],
                        length(get.age.brackets.in.range(lower = 40, upper = 50))),
                    
                    # multiply 40-49 from DHS by 50-59 multiplier from older cohort
                    rep((get.transmission.multipliers.DHS.kenya(sex=sex, year=year)["40-49"]*
                             get.transmission.multipliers.older.cohort.kenya(sex=sex)["50-59"]),
                        length(get.age.brackets.in.range(lower = 50, upper = 60))), 
                    
                    # multiply 40-49 from DHS by 60-69 multiplier from older cohort
                    rep((get.transmission.multipliers.DHS.kenya(sex=sex, year=year)["40-49"]*
                             get.transmission.multipliers.older.cohort.kenya(sex=sex)["60-69"]),
                        length(get.age.brackets.in.range(lower = 60, upper = 70))), 
                    
                    # multiply 40-49 from DHS by 70-79 multiplier from older cohort
                    rep((get.transmission.multipliers.DHS.kenya(sex=sex, year=year)["40-49"]*
                             get.transmission.multipliers.older.cohort.kenya(sex=sex)["70-79"]),
                        length(get.age.brackets.in.range(lower = 70, upper = Inf)))
    )
    names(multipliers) = get.age.brackets.in.range(lower=0,upper=Inf)
    multipliers
}


