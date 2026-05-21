library(distributions)
library(boot) 

make.joint.distribution.age.time = function(median.r2.age.15.to.19, # = 1,
                                            median.r2.age.20.to.29, # = 1,
                                            median.r2.age.40.to.49, # = 0.8,
                                            median.r2.age.50.and.over, # = .5,
                                            median.r0.to.r1, # = 1, #time 0 relative to 1
                                            median.r1.to.r2, # = 1, #time 1 relative to 2
                                            median.r3.to.r2, # = 1, #time 3 relative to 2
                                            sd.r2, # = log(2)/2,
                                            sd.r0.to.r1, # = log(2)/2,
                                            sd.r1.to.r2, # = log(2)/2,
                                            sd.r3.to.r2, # = log(2)/2,
                                            corr.15.to.19.with.20.to.29, # = 0.5,
                                            corr.20.to.29.with.40.to.49, # = 0.25,
                                            corr.40.to.49.with.50.and.over, # = 0.5,
                                            age.prefix.names, # = c("age.15.to.19.transmission.multiplier",
                                                              #   "age.20.to.29.transmission.multiplier",
                                                              #   "age.40.to.49.transmission.multiplier",
                                                              #   "age.50.and.over.transmission.multiplier"),
                                            time.suffixes # = c(".0",".1",".2",".3")
                                            ){
    
    var.names = paste0(rep(age.prefix.names,each=length(time.suffixes)),rep(time.suffixes,length(age.prefix.names)))
 
    mean.vector.age.1 = log(c(median.r2.age.15.to.19,
                              median.r0.to.r1,
                              median.r1.to.r2,
                              median.r3.to.r2))
    mean.vector.age.2 = log(c(median.r2.age.20.to.29,
                              median.r0.to.r1,
                              median.r1.to.r2,
                              median.r3.to.r2))
    mean.vector.age.3 = log(c(median.r2.age.40.to.49,
                              median.r0.to.r1,
                              median.r1.to.r2,
                              median.r3.to.r2))
    mean.vector.age.4 = log(c(median.r2.age.50.and.over,
                              median.r0.to.r1,
                              median.r1.to.r2,
                              median.r3.to.r2))
    
    cov.mat.time = diag(c(sd.r2,
                          sd.r0.to.r1,
                          sd.r1.to.r2,
                          sd.r3.to.r2)^2)
    
    corr.mat.age = rbind(c(1, # corr between age 1 and age 1 
                            corr.15.to.19.with.20.to.29, # corr between age 1 and age 2
                            corr.15.to.19.with.20.to.29*corr.20.to.29.with.40.to.49,  # corr between age 1 and age 3
                            corr.15.to.19.with.20.to.29*corr.20.to.29.with.40.to.49*corr.40.to.49.with.50.and.over),  # corr between age 1 and age 4
                         c(corr.15.to.19.with.20.to.29, # corr between age 2 with everything
                           1, 
                           corr.20.to.29.with.40.to.49,  
                           corr.20.to.29.with.40.to.49*corr.40.to.49.with.50.and.over),
                         c(corr.15.to.19.with.20.to.29*corr.20.to.29.with.40.to.49, # corr between age 3 with everything
                           corr.20.to.29.with.40.to.49, 
                           1,  
                           corr.40.to.49.with.50.and.over),
                         c(corr.15.to.19.with.20.to.29*corr.20.to.29.with.40.to.49*corr.40.to.49.with.50.and.over, # corr between age 4 with everything
                           corr.20.to.29.with.40.to.49*corr.40.to.49.with.50.and.over, 
                           corr.40.to.49.with.50.and.over,  
                           1)) 
    
    M = rbind(c(1,1,1,0), # which elements of the mean vector you multiply to get r0
              c(1,0,1,0), # " " r1
              c(1,0,0,0), # " " r2
              c(1,0,0,1)) # " " r3 
    
    new.mean.vector = rbind(M %*% mean.vector.age.1,
                            M %*% mean.vector.age.2,
                            M %*% mean.vector.age.3,
                            M %*% mean.vector.age.4)

    new.cov.mat.time = M %*% cov.mat.time %*% t(M)
    
    new.cov.mat = rbind(
        cbind(new.cov.mat.time*corr.mat.age[1,1],
              new.cov.mat.time*corr.mat.age[1,2],
              new.cov.mat.time*corr.mat.age[1,3],
              new.cov.mat.time*corr.mat.age[1,4]),
        cbind(new.cov.mat.time*corr.mat.age[2,1],
              new.cov.mat.time*corr.mat.age[2,2],
              new.cov.mat.time*corr.mat.age[2,3],
              new.cov.mat.time*corr.mat.age[2,4]),
        cbind(new.cov.mat.time*corr.mat.age[3,1],
              new.cov.mat.time*corr.mat.age[3,2],
              new.cov.mat.time*corr.mat.age[3,3],
              new.cov.mat.time*corr.mat.age[3,4]),
        cbind(new.cov.mat.time*corr.mat.age[4,1],
              new.cov.mat.time*corr.mat.age[4,2],
              new.cov.mat.time*corr.mat.age[4,3],
              new.cov.mat.time*corr.mat.age[4,4])
    )
    
    Multivariate.Lognormal.Distribution(mu = new.mean.vector,
                                        sigma = new.cov.mat,
                                        var.names = var.names)
    
       
}

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






