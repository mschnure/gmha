source('model/age_mappings.R')

## see "DHS_tables.xlsx" to see where these come from 

get.testing.model.uganda = function(){
    
    model.ages = parse.age.brackets(age.cutoffs = MODEL.AGE.CUTOFFS)
    midpoint.model.ages = sapply(1:length(model.ages$labels), function(x){
        mean(c(model.ages$uppers[[x]],model.ages$lowers[[x]]))}) 
    midpoint.model.ages[length(midpoint.model.ages)] = 85
    
    testing.sexes = c("female","male")
    
    testing.anchor.year = 2000
    testing.data.years =c(2000,2006,2016)-testing.anchor.year
    testing.data.ages = c(17,22,27,35,45) # midpoint of age groups:  15-19, 20-24, 25-29, 30-39, 40-49

    prop.2000.female = c(.060,.114,.111,.076,.055) 
    prop.2000.male = c(.032,.127,.185,.155,.124) 
    prop.2006.female = c(.091,.157,.128,.122,.101) 
    prop.2006.male = c(.046,.128,.184,.113,.079) 
    prop.2016.female = c(.394,.665,.644,.564,.476) 
    prop.2016.male = c(.284,.531,.584,.544,.445) 
    
    max.proportion = 0.85 # ARBITRARY FOR NOW
    
    master.df = data.frame(year=rep(testing.data.years,each=2*length(testing.data.ages)),
                           prop = c(prop.2000.female,prop.2000.male,
                                    prop.2006.female,prop.2006.male,
                                    prop.2016.female,prop.2016.male),
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




