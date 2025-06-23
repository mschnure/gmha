source('model/age_mappings.R')

## Bruyand et al, 2019 (data from 2016, by sex*age)
## Sicsic et al, 2016 (data from 2006-2013, by year, sex, age all one-way)
get.testing.model.france = function(){
    
    model.ages = parse.age.brackets(age.cutoffs = MODEL.AGE.CUTOFFS)
    midpoint.model.ages = sapply(1:length(model.ages$labels), function(x){
        mean(c(model.ages$uppers[[x]],model.ages$lowers[[x]]))}) 
    midpoint.model.ages[length(midpoint.model.ages)] = 85
    
    testing.sexes = c("female","male")
    
    testing.anchor.year = 2000
    
    testing.data.years = c(2016,2006:2013)-testing.anchor.year
    testing.data.ages = c(21,32,47,65, # midpoint of age groups: 18-24, 25-39, 40-54, 55-75
                          22,37,57.5) # midpoint of age groups: 15-29,30-44,45-70
                          
    prop.2016.female = c(.30,.21,.09,.03)
    prop.2016.male = c(.23,.17,.10,.05)

    # 2006-2013
    total = c(.0424,.0445,.0441,.0450,.0475,.0519,.0532,.0581)
    male = c(.0415,.0434,.0420,.0420,.0444,.0475,.0473,.0497)
    female = c(.0432,.0456,.0462,.0479,.0504,.0560,.0589,.0660)
    age1 = c(.0588,.0654,.0660,.0673,.0697,.0777,.0842,.0949) # 15-29
    age2 = c(.0548,.0574,.0570,.0582,.0621,.0714,.0729,.0819) # 30-44
    age3 = c(.0261,.0267,.0263,.0274,.0293,.0301,.0299,.0312) # 45-70
    
    weighted.age = (.214*22 + .291*37 + .495*57.5) # get weighted age average from percentages in each age group (table 1)

    max.proportion = 1 # changed for France # 0.85
    
    master.df = data.frame(year=c(rep(testing.data.years[1],8),
                                  rep(testing.data.years[2:9],6)),
                           prop = c(prop.2016.female,prop.2016.male,
                                    total,male,female,age1,age2,age3),
                           age = c(rep(testing.data.ages[1:4],2),
                                   rep(weighted.age,24),
                                   rep(22,8),
                                   rep(37,8),
                                   rep(57.5,8)),
                           sex=c(rep(c("female","male"),each=4),
                                 rep("total",8),
                                 rep("male",8),
                                 rep("female",8),
                                 rep("total",24)))
    
    intercepts = sapply(testing.sexes,function(sex){
        
        fit = suppressWarnings(glm(prop ~ year*age, family=binomial, data = master.df[master.df$sex==sex,])) # Betas will be intercept, year, age, year:age
        
        sapply(midpoint.model.ages,function(age){
                fit$coefficients[1] + fit$coefficients["age"]*age  
        })
    })
    
    dim(intercepts) = c(dim(intercepts),1)
    dimnames(intercepts)=list(age=model.ages$labels, 
                              sex=testing.sexes,
                              subgroups="all")
    
    # slopes are no longer sex-specific; age-specific instead (if only one sex selected, there is no age*year difference)
    slopes = sapply(testing.sexes,function(sex){
        
        fit = suppressWarnings(glm(prop ~ year*age, family=binomial, data = master.df)) # [master.df$sex==sex,]
        
        sapply(midpoint.model.ages,function(age){
                fit$coefficients["year"] + fit$coefficients["year:age"]*age 
        })
    })
    
    dim(slopes) = c(dim(slopes),1)
    dimnames(slopes)=list(age=model.ages$labels,
                          sex=testing.sexes,
                          subgroups="all")

    projection.times = 1980:2040
    dim.names.projection = list(year = projection.times - testing.anchor.year,
                                age = model.ages$labels,
                                sex = c("male","female"))
    
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
    
    rv = list(intercepts=intercepts,
              slopes=slopes,
              anchor.year=testing.anchor.year,
              max.proportion=max.proportion,
              master.df = master.df,
              predictions=predictions)
    rv
    
}




