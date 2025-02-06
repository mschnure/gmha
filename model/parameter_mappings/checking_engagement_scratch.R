
# 20-29, female, 2005
projected.log.odds = engagement.model$intercept + 
    engagement.model$slope*(2005-engagement.model$anchor.year) + 
    engagement.model$age.20.29 + 
    engagement.model$sex.female

projected.p = 1/(1+exp(-projected.log.odds)) 
projected.rate = -log(1-projected.p)
projected.p

engagement.model$predictions["2005","20-29","female"]

# 10-19, male, 2023
projected.log.odds = engagement.model$intercept + 
    engagement.model$slope*(2023-engagement.model$anchor.year) + 
    engagement.model$age.10.19 

projected.p = 1/(1+exp(-projected.log.odds)) 
projected.rate = -log(1-projected.p)
projected.p

engagement.model$predictions["2023","10-19","male"]


# 30-39, female, 2000
projected.log.odds = engagement.model$intercept + 
    engagement.model$slope*(2000-engagement.model$anchor.year) + 
    engagement.model$sex.female

projected.p = 1/(1+exp(-projected.log.odds)) 
projected.rate = -log(1-projected.p)
projected.p

engagement.model$predictions["2000","30-39","female"]
