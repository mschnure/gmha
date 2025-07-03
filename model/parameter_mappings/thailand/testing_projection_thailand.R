source('model/age_mappings.R')

# 2001 estimate is from Lertpiriyasuwat et al, 2003: cross-sectional survey of adults in Nakhonsawan province 
# 2006 estimate is from 2006 National Sexual Behavior Survey 
get.testing.model.thailand = function(){
    
    model.ages = parse.age.brackets(age.cutoffs = MODEL.AGE.CUTOFFS)
    midpoint.model.ages = sapply(1:length(model.ages$labels), function(x){
        mean(c(model.ages$uppers[[x]],model.ages$lowers[[x]]))}) 
    midpoint.model.ages[length(midpoint.model.ages)] = 85
    
    testing.sexes = c("female","male")
    
    testing.anchor.year = 2000
    testing.data.years =c(2001,2006)-testing.anchor.year
    testing.data.ages = c(17,22,27,35,45) # no ages here, but keeping the format the same 
 
    prop.1.female = c(rep(.19,5)) # earliest year (e.g., 2005)
    prop.1.male = prop.1.female
    prop.2.female = c(rep(.32,5)) # middle year (e.g. 2010) 
    prop.2.male = prop.2.female
    
    max.proportion = 0.85 # ARBITRARY FOR NOW
    
    master.df = data.frame(year=rep(testing.data.years,each=2*length(testing.data.ages)),
                           prop = c(prop.1.female,prop.1.male,
                                    prop.2.female,prop.2.male),
                           age = rep(testing.data.ages,2),
                           sex=rep(c("female","male"),each=length(testing.data.ages)))
    
    intercepts = sapply(testing.sexes,function(sex){
        
        fit = suppressWarnings(glm(prop ~ year, family=binomial, data = master.df[master.df$sex==sex,])) # Betas will be intercept, year
        
        sapply(midpoint.model.ages,function(age){
                fit$coefficients[1] 
        })
    })
    
    dim(intercepts) = c(dim(intercepts),1)
    dimnames(intercepts)=list(age=model.ages$labels, 
                              sex=testing.sexes,
                              subgroups="all")
    
    
    slopes = sapply(testing.sexes,function(sex){
        
        fit = suppressWarnings(glm(prop ~ year, family=binomial, data = master.df[master.df$sex==sex,])) 
        
        sapply(midpoint.model.ages,function(age){
                fit$coefficients["year"] 
        })
    })
    
    dim(slopes) = c(dim(slopes),1)
    dimnames(slopes)=list(age=model.ages$labels,
                          sex=testing.sexes,
                          subgroups="all")
    
    projection.times = 1980:2040
    dim.names.projection = list(age = model.ages$labels,
                                sex = c("male","female"),
                                year = projection.times - testing.anchor.year)
    
    x = sapply(1:length(projection.times), function(year){
        
        projected.log.odds = (intercepts)+
            ((slopes)*(projection.times[year]-testing.anchor.year))
        
        projected.p = 1/(1+exp(-projected.log.odds))
        projected.p = projected.p*max.proportion
        
        projected.p
    })
    
    predictions = array(x,
                        dim = sapply(dim.names.projection,length),
                        dimnames = dim.names.projection)
    
    dimnames(predictions)$year = as.numeric(dimnames(predictions)$year) + testing.anchor.year
    
    predictions = aperm(predictions,c(3,1,2))
    
    rv = list(intercepts=intercepts,
              slopes=slopes,
              anchor.year=testing.anchor.year,
              max.proportion=max.proportion,
              master.df = master.df,
              predictions=predictions)
    rv
    
}
