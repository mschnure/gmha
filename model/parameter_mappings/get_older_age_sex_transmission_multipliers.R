


get.transmission.multipliers.older.cohort.south.africa = function(sex){
    if(sex=="female"){
        data = data.frame(
            ages = c("40-49","50-59","60-69","70-79"),
            prop.high.risk.sex=c(.015,.015,.003,.003), 
            # data says 50s and 70s have 0% multiple partners; that won't work, so instead, using 40s value for 50s and 60s value for 70s
            # condom.use.among.high.risk.sex = c(.165,.102,0,0)) # making 50s value the same as 40s - I don't like that it increases for all
            condom.use.among.high.risk.sex = c(.165,.165,0,0)) 
    } else if(sex=="male"){
        data = data.frame(
            ages = c("40-49","50-59","60-69","70-79"), # making 50s value the same as 40s - I don't like that it increases for all
            #prop.high.risk.sex=c(.195,.197,.123,.083),
            prop.high.risk.sex=c(.195,.195,.123,.083),
            #condom.use.among.high.risk.sex = c(.2,.06,.056,.018)
            condom.use.among.high.risk.sex = c(.2,.2,.056,.018)) 
    }
    
    data$prop.at.risk = data$prop.high.risk.sex*(1-data$condom.use.among.high.risk.sex)
    data$multiplier = data$prop.at.risk/(data$prop.at.risk[data$ages=="40-49"])
    
    rv = array(data$multiplier,
               dim = length(data$ages),
               dimnames = list(data$ages))
    
    rv
    
}