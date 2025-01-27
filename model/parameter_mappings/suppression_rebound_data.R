
##--------------------------------------##
##-- Suppression - Njuguna et al 2022 --##
##--------------------------------------##

# Suppression - Njuguna et al
# prop suppressed at 18 months
suppressed.p = 3227/(3227+741)
suppressed.r = -log(1-suppressed.p)/(18/12)

suppressed.p.12.months = 1-exp(-suppressed.r) # 0.6732885
#-log(1-suppressed.p.12.months)
suppressed.r # 1.118678

time.to.suppress = 1/suppressed.r
time.to.suppress # 0.8939124

## 10/22/2024: realized that I used this probability of suppression (0.6732885) as a rate in the prior distributions; 
## I should have used the direct rate (1.118678) and then the time to suppress would have been 0.8939124, not 1.49
## CORRECTED 12/23/2024

##--------------------------------------##
##-- Viral rebound - Maina et al 2020 --##
##--------------------------------------##
rate.of.viral.rebound.per.100.person.months = mean(c(3.9,0.7,0.89))

rate.of.viral.rebound.one.year = (rate.of.viral.rebound.per.100.person.months/100)*12

viral.rebound.p.one.year = 1-exp(-(rate.of.viral.rebound.one.year))
#viral.rebound.p.one.year # 0.1971601
rate.of.viral.rebound.one.year #0.2196

time.to.rebound = 1/rate.of.viral.rebound.one.year
time.to.rebound # 4.553734

## 10/22/2024: realized that I used this probability of rebound (0.1971601) as a rate in the model and paper; 
## I should have used the direct rate (0.2196) and then the time to rebound would have been 4.55, not 5.07 - fix this in new model! 
## CORRECTED 12/23/2024
