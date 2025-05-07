source("model/parameter_mappings/get_older_age_sex_transmission_multipliers.R")

# Ethiopia: 2005, 2011, 2016
# DRC: 2007
# Cote d'Ivoire: 2012, 2021
# Ghana: 2008, 2022

# averages: 
# 2003: Ethiopia 2005, DRC 2007 
# 2008: Ethiopia 2011, CI 2012, Ghana 2008
# 2014: Ethiopia 2016, CI 2021, Ghana 2022 

get.transmission.multipliers.unaids = function(sex,
                                               year){
    if(sex=="female"){
        if(year==2014){ 
            mult.1 = get.transmission.multipliers.DHS.ethiopia(sex="female",year=2016)
            mult.2 = get.transmission.multipliers.DHS.cote.divoire(sex="female",year=2021)
            mult.3 = get.transmission.multipliers.DHS.ghana(sex="female",year=2022)
            
            rv = rowMeans(cbind(mult.1,mult.2,mult.3))
        } else if(year==2008){
            mult.1 = get.transmission.multipliers.DHS.ethiopia(sex="female",year=2011)
            mult.2 = get.transmission.multipliers.DHS.cote.divoire(sex="female",year=2012)
            mult.3 = get.transmission.multipliers.DHS.ghana(sex="female",year=2008)
            
            rv = rowMeans(cbind(mult.1,mult.2,mult.3))
        } else if(year==2003){
            mult.1 = get.transmission.multipliers.DHS.ethiopia(sex="female",year=2005)
            mult.2 = get.transmission.multipliers.DHS.drc(sex="female",year=2007)
            
            rv = rowMeans(cbind(mult.1,mult.2))
        }
    } else if(sex=="male"){
        if(year==2014){ 
            mult.1 = get.transmission.multipliers.DHS.ethiopia(sex="male",year=2016)
            mult.2 = get.transmission.multipliers.DHS.cote.divoire(sex="male",year=2021)
            mult.3 = get.transmission.multipliers.DHS.ghana(sex="male",year=2022)
            
            rv = rowMeans(cbind(mult.1,mult.2,mult.3))
        } else if(year==2008){
            mult.1 = get.transmission.multipliers.DHS.ethiopia(sex="male",year=2011)
            mult.2 = get.transmission.multipliers.DHS.cote.divoire(sex="male",year=2012)
            mult.3 = get.transmission.multipliers.DHS.ghana(sex="male",year=2008)
            
            rv = rowMeans(cbind(mult.1,mult.2,mult.3))
        } else if(year==2003){
            mult.1 = get.transmission.multipliers.DHS.ethiopia(sex="male",year=2005)
            mult.2 = get.transmission.multipliers.DHS.drc(sex="male",year=2007)
            
            rv = rowMeans(cbind(mult.1,mult.2))
        }
    }
    rv
}

# see DHS_tables.xlsx for what tables these came from 
get.transmission.multipliers.DHS.ethiopia  = function(sex,
                                                    year){
    if(sex=="female"){
        if(year==2016){ 
            data = data.frame(
                ages = c("15-19","20-24","25-29","30-39","40-49"),
                prop.high.risk.sex=c(.003,.003,.005,.002,.005), 
                condom.use.among.high.risk.sex = c(.260,.190,.184,.213,.154)) 
        } else if(year==2011){
            data = data.frame(
                ages = c("15-19","20-24","25-29","30-39","40-49"),
                prop.high.risk.sex=c(.003,.006,.006,.003,.002), 
                condom.use.among.high.risk.sex = c(0,0,0,0,0)) # no condom use reported
        } else if(year==2005){
            data = data.frame(
                ages = c("15-19","20-24","25-29","30-39","40-49"),
                prop.high.risk.sex=c(.004,.006,.001,.002,.001), # 40-49 is zero so using 0.1
                condom.use.among.high.risk.sex = c(.271,.293,.327,.327,.327)) # 30-39 and 40-49 missing so using 25-29
            
        }
    } else if(sex=="male"){
        if(year==2016){ 
            data = data.frame(
                ages = c("15-19","20-24","25-29","30-39","40-49"),
                prop.high.risk.sex=c(.008,.031,.031,.040,.062),
                condom.use.among.high.risk.sex = c(.569,.624,.628,.610,.402))
        } else if(year==2011){
            data = data.frame(
                ages = c("15-19","20-24","25-29","30-39","40-49"),
                prop.high.risk.sex=c(.005,.016,.026,.054,.080), 
                condom.use.among.high.risk.sex = c(.575,.369,.471,.070,.028)) 
            # 15-19 not reported but 15-24 reported so calculated from average (using naive avg though)
        } else if(year==2005){
            data = data.frame(
                ages = c("15-19","20-24","25-29","30-39","40-49"),
                prop.high.risk.sex=c(.039,.050,.032,.040,.046),
                condom.use.among.high.risk.sex = c(.440,.531,.471,.572,.572)) # 40-49 missing so using 30-39
        }
    }
    
    data$prop.at.risk = data$prop.high.risk.sex*(1-data$condom.use.among.high.risk.sex)
    data$multiplier = data$prop.at.risk/(data$prop.at.risk[data$ages=="30-39"])
    
    rv = array(data$multiplier,
               dim = length(data$ages),
               dimnames = list(data$ages))
    
    rv
}

get.transmission.multipliers.DHS.drc  = function(sex,
                                                 year){
    if(sex=="female"){
        if(year==2007){ 
            data = data.frame(
                ages = c("15-19","20-24","25-29","30-39","40-49"),
                prop.high.risk.sex=c(.030,.036,.028,.026,.026), 
                condom.use.among.high.risk.sex = c(.125,.228,.152,.158,.144)) 
        } 
    } else if(sex=="male"){
        if(year==2007){ 
            data = data.frame(
                ages = c("15-19","20-24","25-29","30-39","40-49"),
                prop.high.risk.sex=c(.098,.194,.217,.172,.163),
                condom.use.among.high.risk.sex = c(.219,.310,.283,.282,.186))
        }
    }
    
    data$prop.at.risk = data$prop.high.risk.sex*(1-data$condom.use.among.high.risk.sex)
    data$multiplier = data$prop.at.risk/(data$prop.at.risk[data$ages=="30-39"])
    
    rv = array(data$multiplier,
               dim = length(data$ages),
               dimnames = list(data$ages))
    
    rv
}

get.transmission.multipliers.DHS.cote.divoire  = function(sex,
                                                      year){
    if(sex=="female"){
        if(year==2021){ 
            data = data.frame(
                ages = c("15-19","20-24","25-29","30-39","40-49"),
                prop.high.risk.sex=c(.038,.057,.047,.027,.014), 
                condom.use.among.high.risk.sex = c(.318,.217,.207,.201,.080)) 
        } else if(year==2012){
            data = data.frame(
                ages = c("15-19","20-24","25-29","30-39","40-49"),
                prop.high.risk.sex=c(.043,.053,.041,.023,.013), 
                condom.use.among.high.risk.sex = c(.318,.363,.294,.234,.064)) 
        } 
    } else if(sex=="male"){
        if(year==2021){ 
            data = data.frame(
                ages = c("15-19","20-24","25-29","30-39","40-49"),
                prop.high.risk.sex=c(.078,.229,.205,.195,.177),
                condom.use.among.high.risk.sex = c(.516,.497,.436,.456,.475))
        } else if(year==2012){
            data = data.frame(
                ages = c("15-19","20-24","25-29","30-39","40-49"),
                prop.high.risk.sex=c(.131,.321,.361,.321,.290), 
                condom.use.among.high.risk.sex = c(.701,.509,.402,.266,.124)) 
        }
    }
    
    data$prop.at.risk = data$prop.high.risk.sex*(1-data$condom.use.among.high.risk.sex)
    data$multiplier = data$prop.at.risk/(data$prop.at.risk[data$ages=="30-39"])
    
    rv = array(data$multiplier,
               dim = length(data$ages),
               dimnames = list(data$ages))
    
    rv
}

get.transmission.multipliers.DHS.ghana  = function(sex,
                                                   year){
    if(sex=="female"){
        if(year==2022){ 
            data = data.frame(
                ages = c("15-19","20-24","25-29","30-39","40-49"),
                prop.high.risk.sex=c(.026,.034,.033,.015,.008), 
                condom.use.among.high.risk.sex = c(.172,.115,.105,.066,.009)) 
        } else if(year==2008){
            data = data.frame(
                ages = c("15-19","20-24","25-29","30-39","40-49"),
                prop.high.risk.sex=c(.012,.016,.017,.005,.002),
                condom.use.among.high.risk.sex = c(.244,.311,.313,.110,.063))   
        }
    } else if(sex=="male"){
        if(year==2022){ 
            data = data.frame(
                ages = c("15-19","20-24","25-29","30-39","40-49"),
                prop.high.risk.sex=c(.050,.165,.236,.188,.143),
                condom.use.among.high.risk.sex = c(.217,.316,.318,.271,.221))
        }  else if(year==2008){
            data = data.frame(
                ages = c("15-19","20-24","25-29","30-39","40-49"),
                prop.high.risk.sex=c(.031,.096,.167,.155,.124),
                condom.use.among.high.risk.sex = c(.403,.489,.493,.450,.271)) 
        }
    }
    
    data$prop.at.risk = data$prop.high.risk.sex*(1-data$condom.use.among.high.risk.sex)
    data$multiplier = data$prop.at.risk/(data$prop.at.risk[data$ages=="30-39"])
    
    rv = array(data$multiplier,
               dim = length(data$ages),
               dimnames = list(data$ages))
    
    rv
}

get.all.transmission.multipliers.unaids = function(sex,
                                                   year){
    multipliers = c(rep(0,length(get.age.brackets.in.range(lower = 0, upper = 15))),
                    rep(get.transmission.multipliers.unaids(sex=sex, year=year)["15-19"],
                        length(get.age.brackets.in.range(lower = 15, upper = 20))),
                    rep(get.transmission.multipliers.unaids(sex=sex, year=year)["20-24"],
                        length(get.age.brackets.in.range(lower = 20, upper = 25))),
                    rep(get.transmission.multipliers.unaids(sex=sex, year=year)["25-29"],
                        length(get.age.brackets.in.range(lower = 25, upper = 30))),
                    rep(get.transmission.multipliers.unaids(sex=sex, year=year)["30-39"],
                        length(get.age.brackets.in.range(lower = 30, upper = 40))),
                    rep(get.transmission.multipliers.unaids(sex=sex, year=year)["40-49"],
                        length(get.age.brackets.in.range(lower = 40, upper = 50))),
                    
                    # multiply 40-49 from DHS by 50-59 multiplier from older cohort
                    rep((get.transmission.multipliers.unaids(sex=sex, year=year)["40-49"]*
                             get.transmission.multipliers.older.cohort.south.africa(sex=sex)["50-59"]),
                        length(get.age.brackets.in.range(lower = 50, upper = 60))), 
                    
                    # multiply 40-49 from DHS by 60-69 multiplier from older cohort
                    rep((get.transmission.multipliers.unaids(sex=sex, year=year)["40-49"]*
                             get.transmission.multipliers.older.cohort.south.africa(sex=sex)["60-69"]),
                        length(get.age.brackets.in.range(lower = 60, upper = 70))), 
                    
                    # multiply 40-49 from DHS by 70-79 multiplier from older cohort
                    rep((get.transmission.multipliers.unaids(sex=sex, year=year)["40-49"]*
                             get.transmission.multipliers.older.cohort.south.africa(sex=sex)["70-79"]),
                        length(get.age.brackets.in.range(lower = 70, upper = Inf)))
    )
    names(multipliers) = get.age.brackets.in.range(lower=0,upper=Inf)
    multipliers
}



