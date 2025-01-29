source("model/parameter_mappings/kenya/testing_projection_kenya.R")
source ("model/parameter_mappings/south_africa/testing_projection_sa.R")

get.testing.model = function(location){
    # if(location!="Kenya")
    #     print("Using Kenya-specific testing model for now")
    
    if(location=="Kenya"){
        rv = get.testing.model.kenya()        
    } else if(location=="South Africa"){
        rv = get.testing.model.south.africa()        
    } else 
        rv = get.testing.model.kenya() # using kenya model for all other countries for now 

    rv
}

generate.testing.parameter.table = function(locations){
    
    rv = list()
    
    for(location in locations){
        master.df = get.testing.model(location=location)$master.df
        max.proportion = get.testing.model(location=location)$max.proportion
        
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
        young.p = young.p*max.proportion # probability, age <=17
        
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
        old.p = old.p*max.proportion # probability, age 35-59
        
        old.year.OR = exp(fit.old$coefficients["year"])*exp(fit.old$coefficients["year:age"]*old.age) # OR by year for older age 
        young.year.OR = exp(fit.young$coefficients["year"]) # OR by year for younger age
        old.age.OR = exp(fit.old$coefficients["age"])  # OR by age for older age

        table = data.frame(
            Parameter = c("Probability of receiving an HIV test, Female, age 35–39, 2015",
                          "Probability of receiving an HIV test, Female, age 17 and younger, 2015",
                          "Yearly change in odds of receiving a test, for individuals over 17 (odds ratio)",
                          "Yearly change in odds of receiving a test, for individuals age 17 and under (odds ratio)",
                          "Change in odds of receiving a test for every one-year increase in age, for individuals over 17 (odds ratio)",
                          "Ratio of testing rates among male versus female"),
            Estimate.lower.upper=c(paste0(round(old.p,2), " (",round(((lower.old.odds/(1+lower.old.odds))*max.proportion),2),"-",
                                              round(((upper.old.odds/(1+upper.old.odds))*max.proportion),2),")"),
                                   paste0(round(young.p,2), " (",round(((lower.young.odds/(1+lower.young.odds))*max.proportion),2),"-",
                                          round(((upper.young.odds/(1+upper.young.odds))*max.proportion),2),")"),
                                   paste0(round(old.year.OR,2), " (",round(old.year.OR/4,2),"-",
                                          round(old.year.OR*4,2),")"),
                                   paste0(round(young.year.OR,2), " (",round(young.year.OR/4,2),"-",
                                          round(young.year.OR*4,2),")"),
                                   paste0(round(old.age.OR,2), " (",round(old.age.OR/4,2),"-",
                                          round(old.age.OR*4,2),")"),
                                   paste0("1 (0.25-4)")))
        
        rv[[location]] = table
    }
    
    rv
   
}
