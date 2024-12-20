source('model/age_mappings.R')

get.testing.model = function(location){
    # if(location!="Kenya")
    #     print("Using Kenya-specific testing model for now")
    
    rv = get.testing.model.kenya()
    rv
}

get.testing.model.kenya = function(){
    
    model.ages = parse.age.brackets(age.cutoffs = MODEL.AGE.CUTOFFS)
    midpoint.model.ages = sapply(1:length(model.ages$labels), function(x){
        mean(c(model.ages$uppers[[x]],model.ages$lowers[[x]]))}) 
    midpoint.model.ages[length(midpoint.model.ages)] = 85
    
    testing.sexes = c("female","male")
    
    testing.anchor.year = 2000
    testing.data.years =c(2003,2008,2014)-testing.anchor.year
    testing.data.ages = c(17,22,27,35,45)
    
    prop.2003.female = c(.041,.093,.087,.074,.039)
    prop.2003.male = c(.036,.091,.107,.093,.063)
    prop.2008.female = c(.178,.386,.402,.304,.194)
    prop.2008.male = c(.131,.254,.311,.263,.212)
    prop.2014.female = c(.353,.64,.634,.544,.431)
    prop.2014.male = c(.266,.536,.578,.5,.419)
    
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
                         max.proportion=max.proportion)
    rv
    
}

if(1==2){
    # For parameters table 
    # Young
    sex="female"
    fit.young = suppressWarnings(glm(prop ~ year, family=binomial, data = master.df[master.df$sex==sex & master.df$age==17,]))
    year = 2015
    young.log.odds  = fit.young$coefficients[1] + 
        fit.young$coefficients["year"]*(2015-testing.anchor.year)
    young.odds = exp(young.log.odds)
    lower.young.odds = young.odds/4
    upper.young.odds = young.odds*4
    young.p = young.odds/(1+young.odds)
    young.p = young.p*TESTING.MODEL$max.proportion
    
    # Old
    fit.old = suppressWarnings(glm(prop ~ year*age, family=binomial, data = master.df[master.df$sex==sex & master.df$age!=17,])) 
    old.age=35
    old.log.odds = fit.old$coefficients[1] + # intercept 
        fit.old$coefficients["year"]*(2015-testing.anchor.year) + # old*year
        fit.old$coefficients["age"]*old.age + # old*age
        fit.old$coefficients["year:age"]*(2015-testing.anchor.year)*old.age # old*year*age
    old.odds = exp(old.log.odds)
    lower.old.odds = old.odds/4
    upper.old.odds = old.odds*4
    old.p = old.odds/(1+old.odds)
    old.p = old.p*TESTING.MODEL$max.proportion
    
    
    # For parameters table 
    # probability, age 35-59
    c(old.p, (lower.old.odds/(1+lower.old.odds))*TESTING.MODEL$max.proportion, (upper.old.odds/(1+upper.old.odds))*TESTING.MODEL$max.proportion) 
    # OR by year for older age 
    old.year.OR = exp(fit.old$coefficients["year"])*exp(fit.old$coefficients["year:age"]*old.age) 
    c(old.year.OR,old.year.OR/4,old.year.OR*4)
    # OR by age for older age
    old.age.OR = exp(fit.old$coefficients["age"]) 
    c(old.age.OR, old.age.OR/4, old.age.OR*4)
    # probability, age 17 and under
    c(young.p, (lower.young.odds/(1+lower.young.odds))*TESTING.MODEL$max.proportion, (upper.young.odds/(1+upper.young.odds))*TESTING.MODEL$max.proportion) 
    # OR by year for younger age
    young.year.OR = exp(fit.young$coefficients["year"]) 
    c(young.year.OR,young.year.OR/4,young.year.OR*4)
    
    x = NULL
    for(i in 1:simset@n.sim){
        x[i] = simset@simulations[[i]]$parameters$time.varying.parameters$TESTING.RATES$values[41][[1]]["35-39","female",]
    }
    quantile(x,probs = c(.025,.5,.975))
    
}




