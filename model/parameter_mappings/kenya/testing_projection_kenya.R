source('model/age_mappings.R')

get.testing.model.kenya = function(){
    
    model.ages = parse.age.brackets(age.cutoffs = MODEL.AGE.CUTOFFS)
    midpoint.model.ages = sapply(1:length(model.ages$labels), function(x){
        mean(c(model.ages$uppers[[x]],model.ages$lowers[[x]]))}) 
    midpoint.model.ages[length(midpoint.model.ages)] = 85
    
    testing.sexes = c("female","male")
    
    testing.anchor.year = 2000
    testing.data.years =c(2003,2008,2014)-testing.anchor.year
    testing.data.ages = c(17,22,27,35,45) # midpoint of age groups:  15-19, 20-24, 25-29, 30-39, 40-49

    prop.2003.female = c(.041,.093,.087,.074,.039) # table 12.10 
    prop.2003.male = c(.036,.091,.107,.093,.063) # table 12.10 
    prop.2008.female = c(.178,.386,.402,.304,.194) # table 13.9.1
    prop.2008.male = c(.131,.254,.311,.263,.212) # table 13.9.2
    prop.2014.female = c(.353,.64,.634,.544,.431) # table 13.12.1
    prop.2014.male = c(.266,.536,.578,.5,.419) # table 13.12.2
    
    max.proportion = 0.85 # ARBITRARY FOR NOW
    
    master.df = data.frame(year=rep(testing.data.years,each=2*length(testing.data.ages)),
                           prop = c(prop.2003.female,prop.2003.male,
                                    prop.2008.female,prop.2008.male,
                                    prop.2014.female,prop.2014.male),
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




