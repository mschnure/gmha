library(distributions)
library(boot) 

# updated 4/17/26 to use for age multipliers
make.joint.distribution = function(median.r2, # anchor on trate.2
                                   sd.r2,
                                   median.r0.to.r1, #0 relative to 1
                                   sd.r0.to.r1,
                                   median.r1.to.r2, #1 relative to 2
                                   sd.r1.to.r2,
                                   median.r3.to.r2, #3 relative to 2
                                   sd.r3.to.r2, 
                                   var.names) # c("trate.0","trate.1","trate.2","trate.3")
{
    
    mean.vector = log(c(median.r2,
                        median.r0.to.r1,
                        median.r1.to.r2,
                        median.r3.to.r2))
    cov.mat = diag(c(sd.r2,
                     sd.r0.to.r1,
                     sd.r1.to.r2,
                     sd.r3.to.r2)^2)
    
    M = rbind(c(1,1,1,0), # which elements of the mean vector you multiply to get r0
              c(1,0,1,0), # " " r1
              c(1,0,0,0), # " " r2
              c(1,0,0,1)) # " " r3 
    
    new.mean.vector = M %*% mean.vector
    new.cov.mat = M %*% cov.mat %*% t(M)
    
    Multivariate.Lognormal.Distribution(mu = new.mean.vector,
                                        sigma = new.cov.mat,
                                        var.names = var.names)
    
}


# updated 4/17/26 to use for age multipliers
make.joint.distribution.4 = function(median.r2, # anchor on trate.2
                                   sd.r2,
                                   median.r0.to.r1, #0 relative to 1
                                   sd.r0.to.r1,
                                   median.r1.to.r2, #1 relative to 2
                                   sd.r1.to.r2,
                                   median.r3.to.r2, #3 relative to 2
                                   sd.r3.to.r2, 
                                   median.r4.to.r3, #4 relative to 3
                                   sd.r4.to.r3,
                                   var.names) # c("trate.0","trate.1","trate.2","trate.3","trate.4")
{
    
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
                                        var.names = var.names)
    
                                   }

# checking for missing variables
if(1==2){
    
    setdiff(prior@var.names,unlist(parameter.var.blocks)) # everything that is in the first set but not the second - order matters
    setdiff(unlist(parameter.var.blocks),prior@var.names)
    
}






