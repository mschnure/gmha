source("model/parameter_mappings/get_older_age_sex_transmission_multipliers.R")

# see DHS_tables.xlsx for what tables these came from 

get.transmission.multipliers.DHS.mozambique  = function(sex,
                                                        year){
    if(sex=="female"){
        if(year==2022){ 
            data = data.frame(
                ages = c("15-19","20-24","25-29","30-39","40-49"),
                prop.high.risk.sex=c(.038,.044,.052,.049,.034), 
                condom.use.among.high.risk.sex = c(.355,.331,.258,.199,.193)) 
        } else if(year==2011){
            data = data.frame(
                ages = c("15-19","20-24","25-29","30-39","40-49"),
                prop.high.risk.sex=c(.027,.033,.036,.028,.015), 
                condom.use.among.high.risk.sex = c(.425,.341,.405,.168,.128)) 
        } else if(year==2003){
            data = data.frame( 
                ages = c("15-19","20-24","25-29","30-39","40-49"),
                prop.high.risk.sex=c(.501,.270,.180,.151,.134), 
                condom.use.among.high.risk.sex = c(.303,.272,.190,.169,.086)) 
            
        }
    } else if(sex=="male"){
        if(year==2022){ 
            data = data.frame(
                ages = c("15-19","20-24","25-29","30-39","40-49"),
                prop.high.risk.sex=c(.203,.439,.465,.422,.359),
                condom.use.among.high.risk.sex = c(.430,.392,.248,.231,.121))
        } else if(year==2011){
            data = data.frame(
                ages = c("15-19","20-24","25-29","30-39","40-49"),
                prop.high.risk.sex=c(.179,.356,.347,.344,.278), 
                condom.use.among.high.risk.sex = c(.435,.389,.275,.135,.088)) 
        } else if(year==2003){
            data = data.frame( 
                ages = c("15-19","20-24","25-29","30-39","40-49"),
                prop.high.risk.sex=c(.964,.694,.488,.309,.228), 
                condom.use.among.high.risk.sex = c(.299,.384,.341,.287,.383)) 
        }
    }
    
    data$prop.at.risk = data$prop.high.risk.sex*(1-data$condom.use.among.high.risk.sex)
    data$multiplier = data$prop.at.risk/(data$prop.at.risk[data$ages=="30-39"])
    
    rv = array(data$multiplier,
               dim = length(data$ages),
               dimnames = list(data$ages))
    
    rv
}

get.all.transmission.multipliers.mozambique = function(sex,
                                                         year){
    multipliers = c(rep(0,length(get.age.brackets.in.range(lower = 0, upper = 15))),
                    rep(get.transmission.multipliers.DHS.mozambique(sex=sex, year=year)["15-19"],
                        length(get.age.brackets.in.range(lower = 15, upper = 20))),
                    rep(get.transmission.multipliers.DHS.mozambique(sex=sex, year=year)["20-24"],
                        length(get.age.brackets.in.range(lower = 20, upper = 25))),
                    rep(get.transmission.multipliers.DHS.mozambique(sex=sex, year=year)["25-29"],
                        length(get.age.brackets.in.range(lower = 25, upper = 30))),
                    rep(get.transmission.multipliers.DHS.mozambique(sex=sex, year=year)["30-39"],
                        length(get.age.brackets.in.range(lower = 30, upper = 40))),
                    rep(get.transmission.multipliers.DHS.mozambique(sex=sex, year=year)["40-49"],
                        length(get.age.brackets.in.range(lower = 40, upper = 50))),
                    
                    # multiply 40-49 from DHS by 50-59 multiplier from older cohort
                    rep((get.transmission.multipliers.DHS.mozambique(sex=sex, year=year)["40-49"]*
                             get.transmission.multipliers.older.cohort.south.africa(sex=sex)["50-59"]),
                        length(get.age.brackets.in.range(lower = 50, upper = 60))), 
                    
                    # multiply 40-49 from DHS by 60-69 multiplier from older cohort
                    rep((get.transmission.multipliers.DHS.mozambique(sex=sex, year=year)["40-49"]*
                             get.transmission.multipliers.older.cohort.south.africa(sex=sex)["60-69"]),
                        length(get.age.brackets.in.range(lower = 60, upper = 70))), 
                    
                    # multiply 40-49 from DHS by 70-79 multiplier from older cohort
                    rep((get.transmission.multipliers.DHS.mozambique(sex=sex, year=year)["40-49"]*
                             get.transmission.multipliers.older.cohort.south.africa(sex=sex)["70-79"]),
                        length(get.age.brackets.in.range(lower = 70, upper = Inf)))
    )
    names(multipliers) = get.age.brackets.in.range(lower=0,upper=Inf)
    multipliers
}



