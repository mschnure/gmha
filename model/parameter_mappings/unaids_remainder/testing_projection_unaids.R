source('model/age_mappings.R')

## see "DHS_tables.xlsx" to see where these come from 

# Ethiopia: 2005, 2011, 2016
# DRC: 2007
# Cote d'Ivoire: 2012, 2021
# Ghana: 2008, 2022

# averages: 
# year 1 (~2006): Ethiopia 2005, DRC 2007 
# year 2 (~2010): Ethiopia 2011, CI 2012, Ghana 2008
# year 3 (~2020): Ethiopia 2016, CI 2021, Ghana 2022 

get.testing.model.unaids = function(){
    
    model.ages = parse.age.brackets(age.cutoffs = MODEL.AGE.CUTOFFS)
    midpoint.model.ages = sapply(1:length(model.ages$labels), function(x){
        mean(c(model.ages$uppers[[x]],model.ages$lowers[[x]]))}) 
    midpoint.model.ages[length(midpoint.model.ages)] = 85
    
    testing.sexes = c("female","male")
    
    testing.anchor.year = 2000
    testing.data.years =c(2006,2010,2020)-testing.anchor.year
    testing.data.ages = c(17,22,27,35,45) # midpoint of age groups:  15-19, 20-24, 25-29, 30-39, 40-49

    prop.1.female.1 = c(.024,.036,.017,.010,.006) # ethiopia 2005
    prop.1.male.1 = c(.015,.040,.041,.016,.010) 
    prop.1.female.2 = c(.023,.045,.060,.050,.025) # DRC 2007 
    prop.1.male.2 = c(.014,.044,.041,.058,.034)
    
    prop.1.female = rowMeans(cbind(prop.1.female.1,prop.1.female.2))  # earliest year (e.g., 2005)
    prop.1.male = rowMeans(cbind(prop.1.male.1,prop.1.male.2)) 
    
    prop.2.female.1 = c(.188,.245,.231,.192,.139) # ethiopia 2011
    prop.2.male.1 = c(.165,.248,.237,.200,.198)
    prop.2.female.2 = c(.097,.175,.173,.166,.067) # CI 2012
    prop.2.male.2 = c(.052,.098,.089,.122,.105)
    prop.2.female.3 = c(.026,.076,.125,.080,.037) # ghana 2008 
    prop.2.male.3 = c(.016,.057,.047,.057,.029)
    
    prop.2.female = rowMeans(cbind(prop.2.female.1,prop.2.female.2,prop.2.female.3))  # middle year (e.g. 2010)
    prop.2.male = rowMeans(cbind(prop.2.male.1,prop.2.male.2,prop.2.male.3)) 
    
    prop.3.female.1 = c(.124,.249,.244,.203,.167) # ethiopia 2016 
    prop.3.male.1 = c(.089,.228,.276,.204,.177)
    prop.3.female.2 = c(.066,.161,.176,.150,.079) # CI 2021
    prop.3.male.2 = c(.015,.055,.105,.112,.089)
    prop.3.female.3 = c(.042,.168,.256,.191,.091) # ghana 2022
    prop.3.male.3 = c(.011,.050,.106,.097,.079)
    
    prop.3.female = rowMeans(cbind(prop.3.female.1,prop.3.female.2,prop.3.female.3)) # most recent year (e.g. 2015)
    prop.3.male = rowMeans(cbind(prop.3.male.1,prop.3.male.2,prop.3.male.3)) 

    max.proportion = 0.85 # ARBITRARY FOR NOW
    
    master.df = data.frame(year=rep(testing.data.years,each=2*length(testing.data.ages)),
                           prop = c(prop.1.female,prop.1.male,
                                    prop.2.female,prop.2.male,
                                    prop.3.female,prop.3.male),
                           age = rep(testing.data.ages,2),
                           sex=rep(c("female","male"),each=length(testing.data.ages)))
    
    intercepts = sapply(testing.sexes,function(sex){
        
        fit = suppressWarnings(glm(prop ~ year*age, family=binomial, data = master.df[master.df$sex==sex & master.df$age!=17,])) # Betas will be intercept, year, age, year:age
        fit.young = suppressWarnings(glm(prop ~ year, family=binomial, data = master.df[master.df$sex==sex & master.df$age==17,]))
        
        sapply(midpoint.model.ages,function(age){
            if(age>18){
                fit$coefficients[1] + fit$coefficients["age"]*age  
            } else {
                fit.young$coefficients[1]    
            }
        })
    })
    
    dim(intercepts) = c(dim(intercepts),1)
    dimnames(intercepts)=list(age=model.ages$labels, 
                              sex=testing.sexes,
                              subgroups="all")
    
    
    slopes = sapply(testing.sexes,function(sex){
        
        fit = suppressWarnings(glm(prop ~ year*age, family=binomial, data = master.df[master.df$sex==sex & master.df$age!=17,])) 
        fit.young = suppressWarnings(glm(prop ~ year, family=binomial, data = master.df[master.df$sex==sex & master.df$age==17,]))
        
        sapply(midpoint.model.ages,function(age){
            if(age>18){
                fit$coefficients["year"] + fit$coefficients["year:age"]*age 
            } else {
                fit.young$coefficients["year"]
            }
        })
    })
    
    dim(slopes) = c(dim(slopes),1)
    dimnames(slopes)=list(age=model.ages$labels,
                          sex=testing.sexes,
                          subgroups="all")
    
    rv = list(intercepts=intercepts,
              slopes=slopes,
              anchor.year=testing.anchor.year,
              max.proportion=max.proportion,
              master.df = master.df)
    rv
    
}




