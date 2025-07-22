source("model/parameter_mappings/kenya/testing_projection_kenya.R")
source("model/parameter_mappings/south_africa/testing_projection_sa.R")
source("model/parameter_mappings/mozambique/testing_projection_mozambique.R")
source("model/parameter_mappings/tanzania/testing_projection_tanzania.R")
source("model/parameter_mappings/uganda/testing_projection_uganda.R")
source("model/parameter_mappings/zambia/testing_projection_zambia.R")
source("model/parameter_mappings/zimbabwe/testing_projection_zimbabwe.R")
source("model/parameter_mappings/malawi/testing_projection_malawi.R")
source("model/parameter_mappings/nigeria/testing_projection_nigeria.R")
source("model/parameter_mappings/non_unaids_remainder/testing_projection_india.R")
source("model/parameter_mappings/unaids_remainder/testing_projection_unaids.R")
source("model/parameter_mappings/france/testing_projection_france.R")
source("model/parameter_mappings/thailand/testing_projection_thailand.R")
source("model/parameter_mappings/cambodia/testing_projection_cambodia.R")

get.testing.model = function(location){
    if(location=="Kenya"){
        rv = get.testing.model.kenya()        
    } else if(location=="South Africa"){
        rv = get.testing.model.south.africa() 
    } else if(location=="Mozambique"){
        rv = get.testing.model.mozambique() 
    } else if(location=="Tanzania"){
        rv = get.testing.model.tanzania() 
    } else if(location=="Uganda"){
        rv = get.testing.model.uganda() 
    } else if(location=="Zambia"){
        rv = get.testing.model.zambia() 
    } else if(location=="Zimbabwe"){
        rv = get.testing.model.zimbabwe() 
    } else if(location=="Malawi"){
        rv = get.testing.model.malawi() 
    } else if(location=="Nigeria"){
        rv = get.testing.model.nigeria() 
    } else if(location=="non.unaids.remainder"){
        rv = get.testing.model.india() # using India for non-UNAIDS remainder 
    } else if(location=="unaids.remainder"){
        rv = get.testing.model.unaids() 
    } else if(location=="France"){
        rv = get.testing.model.france()
    } else if(location=="Thailand"){
        rv = get.testing.model.thailand()
    } else if(location=="Cambodia"){
        rv = get.testing.model.cambodia()
    } else {
        rv = get.testing.model.kenya() # using kenya model for all other countries for now 
        #print("Using Kenya's testing model")
    }
        

    rv
}

generate.testing.parameter.table = function(locations){
    
    rv = list()
    
    for(location in locations){
        master.df = get.testing.model(location=location)$master.df
        max.proportion = get.testing.model(location=location)$max.proportion
        anchor.year = get.testing.model(location=location)$anchor.year
        
        # Young
        sex="female"
        fit.young = suppressWarnings(glm(prop ~ year, family=binomial, data = master.df[master.df$sex==sex & master.df$age==17,]))
        year = 2015
        young.log.odds  = fit.young$coefficients[1] + 
            fit.young$coefficients["year"]*(2015-anchor.year)
        young.odds = exp(young.log.odds)
        lower.young.odds = young.odds/4
        upper.young.odds = young.odds*4
        young.p = young.odds/(1+young.odds)
        young.p = young.p*max.proportion # probability, age <=17
        
        # Old
        fit.old = suppressWarnings(glm(prop ~ year*age, family=binomial, data = master.df[master.df$sex==sex & master.df$age!=17,])) 
        old.age=35
        old.log.odds = fit.old$coefficients[1] + # intercept 
            fit.old$coefficients["year"]*(2015-anchor.year) + # old*year
            fit.old$coefficients["age"]*old.age + # old*age
            fit.old$coefficients["year:age"]*(2015-anchor.year)*old.age # old*year*age
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

plot.testing.fit.france.thailand = function(location,
                                            dimension = "total" # or age or sex
){
    model = get.testing.model(location)
    
    if(dimension=="age"){
        plot = ggplot() + 
            #geom_point(data=reshape2::melt(apply(model$data.array, c("age","year"),mean)), aes(x=year, y=value, color=age)) + 
            geom_line(data=reshape2::melt(apply(model$predictions, c("age","year"),mean)), aes(x=year, y=value, color=age)) + 
            ylim(0,1) + facet_wrap(~age) 
    } else if(dimension=="sex"){
        plot = ggplot() + 
            #geom_point(data=reshape2::melt(apply(model$data.array, c("sex","year"),mean)), aes(x=year, y=value, color=sex)) + 
            geom_line(data=reshape2::melt(apply(model$predictions, c("sex","year"),mean)), aes(x=year, y=value, color=sex)) + 
            ylim(0,1) + facet_wrap(~sex) 
    } else if(dimension=="total"){
        #data = reshape2::melt(apply(model$data.array, c("year"),mean))
        #data$year = as.numeric(rownames(data))
        
        predictions = reshape2::melt(apply(model$predictions, c("year"),mean))
        predictions$year = as.numeric(rownames(predictions))
        
        plot = ggplot() + 
            #geom_point(data=data, aes(x=year, y=value)) + 
            geom_line(data=predictions, aes(x=year, y=value)) + 
            ylim(0,1) 
    } else  
        stop("can only plot testing fit by total, age, or sex dimension")
    
    plot
    
    
}

# this isn't working right now 
plot.testing.fit = function(location,
                            sex,
                            age){
    testing.model = get.testing.model(location=location)
    master.df = testing.model$master.df
    master.df$year = master.df$year + 2000
    master.df$age[master.df$age < 35] = paste0((master.df$age[master.df$age < 35])-2,"-",master.df$age[master.df$age < 35]+2)
    master.df$age[master.df$age > 34] = paste0((master.df$age[master.df$age > 34]),"-",as.numeric(master.df$age[master.df$age > 34])+4)
    
    testing.times = c(1976:2040)
    testing.rates = c(lapply(testing.times, function(year){
        
        projected.log.odds = (testing.model$intercepts + testing.model$slopes)
        
        projected.p = 1/(1+exp(-projected.log.odds))
        projected.p = projected.p*testing.model$max.proportion
        projected.rate = -log(1-projected.p)
        
        projected.rate
        
    }))
    
    df = master.df[master.df$age==age & master.df$sex==sex,]
    
    projected.value = lapply(testing.rates, function(array) {
        age_index <- which(dimnames(array)[[1]] == age)
        sex_index <- which(dimnames(array)[[2]] == sex)

        if (length(age_index) > 0 && length(sex_index) > 0) {
            mean(array[age_index, sex_index, ], na.rm = TRUE)
        } else {
            NA  
        }
    })
    
    projected.df = data.frame(year=testing.times,
                              value=unlist(projected.value))

    data.df = data.frame(year=c(projected.df$year),
                    value=c(rep(NA,27),df$prop[1],
                        rep(NA,4),df$prop[2],
                        rep(NA,5),df$prop[3],
                        rep(NA,26)))
        
    plot = qplot(c(data.df$year,projected.df$year),
                 c(data.df$value,projected.df$value),
                 color=rep(c("true","fitted"),
                           each=length(projected.df$year)),geom="line")
    
    plot
}
