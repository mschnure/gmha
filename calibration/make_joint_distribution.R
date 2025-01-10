library(distributions)
library(boot) 

# PRIOR is set to country-specific prior at the bottom of this file 

# for most of these, log normal is fine; maybe logit normal for some; types of parameters: rates, ratios
# if lognormal, being off by a factor in either direction is equivalent, rather than absolute value (half versus 2x are the same)
# log(x)/2 means 95% cred interval from 1/x*guess to x*guess

# mean value given to function is log of what I think the value should be (e.g., mean value of log(1) means I think the value is 1)
# without data, best guess would have mean at 0 (or 1) and then decide how far off I'm willing to be 
# log(4)/2 --> can be off by a factor of 4 (=log(2), just keeping this way for clarity)

make.joint.distribution = function(median.r2, # anchor on trate.2
                                   sd.r2,
                                   median.r0.to.r1, #0 relative to 1
                                   sd.r0.to.r1,
                                   median.r1.to.r2, #1 relative to 2
                                   sd.r1.to.r2,
                                   median.r3.to.r2, #3 relative to 2
                                   sd.r3.to.r2, 
                                   median.r4.to.r3, #4 relative to 3
                                   sd.r4.to.r3){
    
    mean.vector = log(c(median.r2,
                      median.r0.to.r1,
                      median.r1.to.r2,
                      median.r3.to.r2,
                      median.r4.to.r3))
    cov.mat = diag(c(sd.r2,
                     sd.r0.to.r1,
                     sd.r1.to.r2,
                     sd.r3.to.r2,
                     sd.r4.to.r3)^2)
    
    M = rbind(c(1,1,1,0,0), # which elements of the mean vector you multiply to get r0
              c(1,0,1,0,0), # " " r1
              c(1,0,0,0,0), # " " r2
              c(1,0,0,1,0), # " " r3
              c(1,0,0,1,1)) # " " r4 
    
    new.mean.vector = M %*% mean.vector
    new.cov.mat = M %*% cov.mat %*% t(M)
    
    Multivariate.Lognormal.Distribution(mu = new.mean.vector,
                                        sigma = new.cov.mat,
                                        var.names = c("trate.0","trate.1","trate.2","trate.3","trate.4"))
    
}

# checking for missing variables
if(1==2){
    
    setdiff(prior@var.names,unlist(parameter.var.blocks)) # everything that is in the first set but not the second - order matters
    setdiff(unlist(parameter.var.blocks),prior@var.names)
    
}






