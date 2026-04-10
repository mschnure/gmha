PLOT.DIR = "manuscript/manuscript_figures/"

PLOT.LIMITS = c("Global" = 6000000,"non.unaids.remainder" = 2250000,"unaids.remainder" = 1500000,
                "South Africa" = 1150000,"Mozambique" = 400000,"Nigeria" = 300000,"Tanzania" = 250000,
                "Uganda" = 250000,"Kenya" = 200000,"Zambia" = 200000,"Zimbabwe" = 200000,"Malawi" = 160000,
                "all.low" = 1000000,"all.lower.middle" = 1600000,"all.upper.middle" = 1750000,"all.high" = 110000
)

#### ORIGINAL AGE DIST FIGURE SETTINGS ####
AGE.DIST.PAL = c(brewer.pal(n=12,"Paired")[2],brewer.pal(n=12,"Paired")[5],brewer.pal(n=12,"Paired")[4]) 
AGE.DIST.ALPHA = 0.8

AGE.DIST.PLOT.WIDTH = 2000
AGE.DIST.PLOT.HEIGHT = 1500
AGE.DIST.PLOT.RES = 200


PLOT.LIMITS.GEN.POP = list("all.low" = c("break.upper"=200000000,
                                         "breaks" = 25000000,
                                         "upper" = 135000000),
                           "all.lower.middle" = c("break.upper"=240000000,
                                                  "breaks" = 25000000,
                                                  "upper" = 235000000),
                           "all.upper.middle" = c("break.upper"=240000000,
                                                  "breaks" = 25000000,
                                                  "upper" = 235000000),
                           "all.high" = c("break.upper"=130000000,
                                                  "breaks" = 25000000,
                                                  "upper" = 135000000))
