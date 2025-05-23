#########################################################################
# Description: Functions to extract simset results after interventions 
#########################################################################
library("scales")
# Functions
#     1. generate.full.results.array
#     2. generate.age.distribution
#     3. generate.age.distribution.2.column
#     4. calculate.median
#     5. calculate.median.age.for.sim
#     6. calculate.percent.over.age.for.sim
#     7. calculate.median.age.for.simset
#     8. calculate.percent.over.age.for.simset
#     9. generate.median.age.table
#     10. generate.percent.over.age.table
#     11. calculate.incidence.reduction


generate.csv = function(summary.results){
    
    outcomes = c("Median age of PWH","% PWH over age 50","# PWH over age 50",
                 "% PWH over age 65","# PWH over age 65",
                 "Year by which 50% of PWH will be age 50+","Year by which 25% of PWH will be age 65+")
    dim.names = list(year = c("2025","2040"),
                     outcomes = outcomes)
    
    export.to.csv = c(summary.results$prevalence.incidence.median.age.table[,,1],
                      summary.results$prevalence.percent.over.50.table,
                      summary.results$prevalence.number.over.50.table,
                      summary.results$prevalence.percent.over.65.table,
                      summary.results$prevalence.number.over.65.table,
                      rep(summary.results$median.over.50.year[2],2),
                      rep(summary.results$quarter.over.65.year[2],2))
    
    dim(export.to.csv) = sapply(dim.names,length)
    dimnames(export.to.csv) = dim.names
    
    export.to.csv = t(export.to.csv)
    
    export.to.csv
}

does.year.contain.threshold.age = function(age.counts,
                                           sim,
                                           statistic.threshold,
                                           age.threshold){
    
    cumulative.probs = cumsum(age.counts)/sum(age.counts)
    age.threshold.bracket = sim$AGES[age.threshold>=sim$parameters$AGE.LOWERS & age.threshold<sim$parameters$AGE.UPPERS]
    
    # Find the index of the first element that is >= the percentile
    index = which((1-cumulative.probs) < statistic.threshold)[1]
    #index = which(cumulative.probs >= (1-statistic.threshold))[1]
    
    (age.threshold.bracket==names(index))
}

pull.year.for.statistic.for.sim = function(sim,
                                           data.type,
                                           statistic.threshold,
                                           age.threshold,
                                           sexes){
    
    age.counts = extract.data(sim = sim,
                              data.type = data.type,
                              year = sim$years[sim$years>1999], # only taking from 2000
                              sexes = sexes,
                              keep.dimensions = c("year","age"))
    year.mask = apply(age.counts,"year",does.year.contain.threshold.age,
                      sim = sim,
                      statistic.threshold = statistic.threshold,
                      age.threshold = age.threshold)
    rv = names(year.mask)[which(year.mask)[1]]
    
    rv
}

pull.year.for.statistic.for.simset = function(simset,
                                              data.type,
                                              statistic.threshold,
                                              age.threshold,
                                              sexes=c("male","female")){
    
    rv = sapply(simset@simulations, function(sim){
        pull.year.for.statistic.for.sim(sim=sim,
                                        data.type=data.type,
                                        statistic.threshold=statistic.threshold,
                                        age.threshold=age.threshold,
                                        sexes=sexes)
    })
    
    if(all(is.na(rv)))
        stop("There is no year by which this statistic is met, try running simset longer ")
    rv = quantile(as.numeric(rv),probs=c(.025,.5,.975), na.rm=T)
    
    rv
}


# full array of results indexed [year,age,sex,outcome,sim,intervention] - each intervention is a simset
generate.full.results.array = function(simset.list,
                                       years = as.character(simset.list[[1]]@simulations[[1]]$years),
                                       ages = simset.list[[1]]@simulations[[1]]$AGES,
                                       sexes = simset.list[[1]]@simulations[[1]]$SEXES,
                                       outcomes = c(
                                           # STATES: 
                                           "population","prevalence","awareness","engagement","suppression", 
                                           # ANNUAL CONTINUUM TRANSITIONS: 
                                           "incidence","diagnoses","annual.engagement","annual.suppression",
                                           "disengagement.unsuppressed","disengagement.suppressed",
                                           # MORTALITY: 
                                           "hiv.mortality","non.hiv.mortality","total.mortality"
                                       )){
    sims = paste0("sim",c(1:simset.list[[1]]@n.sim))
    simset.list = simset.list
    if(is.null(names(simset.list)))
        stop("simset list must be named by intervention")
    interventions = names(simset.list)
    
    full.dim.names = list(year = years,
                          age = ages,
                          sex = sexes,
                          outcome = outcomes,
                          sim = sims,
                          intervention = interventions)
    
    rv = sapply(simset.list, function(simset){
        sapply(simset@simulations, function(sim){
            sapply(outcomes, function(x){
                extract.data(sim, 
                             data.type = x,
                             years = full.dim.names$year,
                             keep.dimensions = c("year","age","sex"))
            })
        })
    })
    
    dim(rv) = sapply(full.dim.names, length)
    dimnames(rv) = full.dim.names
    
    rv
}

plot.age.distribution = function(sim,
                                 year.1 = "2025",
                                 year.2 = "2040",
                                 outcome = "prevalence",
                                 sexes = c("male","female"),
                                 plot.limits=c(0,200000)
){
    
    data = extract.data(sim, 
                        data.type = outcome,
                        years = as.character(sim$years),
                        keep.dimensions = c("year","age","sex"))
    
    dim.names = dimnames(data)
    dim.names.new = c(dim.names[1:2],
                      list(sex = sexes))
    
    data = data[,,sexes]
    dim(data) = sapply(dim.names.new,length)
    dimnames(data) = dim.names.new
    
    if(length(sexes)==1){
        age.counts.1 = data[year.1,,]
        age.counts.2 = data[year.2,,]
    } else {
        age.counts.1 = apply(data[year.1,,],c("age"),sum)
        age.counts.2 = apply(data[year.2,,],c("age"),sum)
    }
    df = data.frame(value = c(age.counts.1,age.counts.2),
                    age = rep(dim.names$age,2),
                    year = rep(c(year.1,year.2),each = length(age.counts.1)))
    
    ggplot(data = df,aes(x=factor(age, levels=unique(age)),y=value,fill=year)) + 
        geom_bar(stat="identity",position = "dodge") + 
        labs(title = paste0(outcome),
             subtitle = paste0(sexes ,collapse=", "))+
        scale_y_continuous(labels = function(x){format(x,big.mark=",")},limits=plot.limits) + 
        theme(panel.background = element_blank(), legend.position = "bottom"
        ) + 
        xlab("Age") 
    
}

# can display % or #; can have two different interventions/years (e.g., no.int/2025 vs. all.max/2040)
generate.age.distribution = function(results.array,
                                     intervention.1,
                                     year.1,
                                     intervention.2,
                                     year.2,
                                     intervention.3,
                                     year.3,
                                     outcome,
                                     percent=T,
                                     sexes=c("female","male"),
                                     display="figure",
                                     plot.limits=c(0,200000)){
    
    if(outcome=="incidence"){
        results.array = results.array[,-c(2:3),,,,]
    }
    
    results.array.1 = results.array[,,sexes,,,intervention.1]
    results.array.2 = results.array[,,sexes,,,intervention.2]
    results.array.3 = results.array[,,sexes,,,intervention.3]
    
    # add back in sex dimension (if only one sex, will collapse over it)
    dim.names = dimnames(results.array)[-length(dimnames(results.array))]
    dim.names$sex = sexes
    dim(results.array.1) = dim(results.array.2) = dim(results.array.3) = sapply(dim.names,length)
    dimnames(results.array.1) = dimnames(results.array.2) = dimnames(results.array.3) = dim.names
    
    # get age counts
    age.counts.1 = apply(results.array.1[year.1,,,outcome,],c("age","sim"),sum)
    age.counts.2 = apply(results.array.2[year.2,,,outcome,],c("age","sim"),sum)
    age.counts.3 = apply(results.array.3[year.3,,,outcome,],c("age","sim"),sum)
    
    if(percent){
        # get totals 
        total.counts.1 = apply(age.counts.1,c("sim"),sum)
        total.counts.2 = apply(age.counts.2,c("sim"),sum)
        total.counts.3 = apply(age.counts.3,c("sim"),sum)
        
        age.proportions.1 = age.counts.1/rep(as.numeric(total.counts.1), each=length(dimnames(age.counts.1)[[1]]))
        age.proportions.2 = age.counts.2/rep(as.numeric(total.counts.2), each=length(dimnames(age.counts.1)[[1]]))
        age.proportions.3 = age.counts.3/rep(as.numeric(total.counts.3), each=length(dimnames(age.counts.1)[[1]]))
        
        # marginalizing over sim now
        age.summary.1 = apply(age.proportions.1,c("age"),quantile,probs=c(.025,.5,.975),na.rm=T)
        age.summary.2 = apply(age.proportions.2,c("age"),quantile,probs=c(.025,.5,.975),na.rm=T)
        age.summary.3 = apply(age.proportions.3,c("age"),quantile,probs=c(.025,.5,.975),na.rm=T)
    } else {
        # marginalizing over sim now
        age.summary.1 = apply(age.counts.1,c("age"),quantile,probs=c(.025,.5,.975),na.rm=T)
        age.summary.2 = apply(age.counts.2,c("age"),quantile,probs=c(.025,.5,.975),na.rm=T)
        age.summary.3 = apply(age.counts.3,c("age"),quantile,probs=c(.025,.5,.975),na.rm=T)
    }
    
    
    if(display=="table"){
        tab.1 = c(paste0(round(100*age.summary.1[2,],1),"% [",
                         round(100*age.summary.1[1,],1),"-",
                         round(100*age.summary.1[3,],1),"]"),
                  paste0(round(100*age.summary.2[2,],1),"% [",
                         round(100*age.summary.2[1,],1),"-",
                         round(100*age.summary.2[3,],1),"]"),
                  paste0(round(100*age.summary.3[2,],1),"% [",
                         round(100*age.summary.3[1,],1),"-",
                         round(100*age.summary.3[3,],1),"]"))
        tab.dim.names.1 = list(age=dimnames(age.counts.1)[[1]],
                               intervention=c(paste0(intervention.1,"/",year.1),
                                              paste0(intervention.2,"/",year.2),
                                              paste0(intervention.3,"/",year.3)))
        dim(tab.1) = sapply(tab.dim.names.1,length)
        dimnames(tab.1) = tab.dim.names.1
        
        return(tab.1)
    } else if(display=="figure"){
        
        age.summary = c(age.summary.1,age.summary.2,age.summary.3)
        dim.names = list(stat = c("lower","median","upper"),
                         age=dimnames(age.counts.1)[[1]],
                         intervention=c(paste0(intervention.1,"/",year.1),
                                        paste0(intervention.2,"/",year.2),
                                        paste0(intervention.3,"/",year.3)))
        dim(age.summary) = sapply(dim.names,length)
        dimnames(age.summary) = dim.names
        
        age.summary = age.summary["median",,]
        df = melt(age.summary)
        
        if(percent){
            ggplot(data = df,aes(x=age,y=value,fill=intervention)) + 
                geom_bar(stat="identity",position = "dodge") + 
                labs(title = paste0(outcome),
                     subtitle = paste0(sexes ,collapse=", "))+
                scale_y_continuous(labels = scales::percent,name = NULL,limits=plot.limits) + 
                theme(panel.background = element_blank(), legend.position = "bottom"
                      # panel.border = element_blank(), axis.line = element_line(color="gray")
                      ) + 
                xlab("Age") 
        } else {
            ggplot(data = df,aes(x=age,y=value,fill=intervention)) + 
                geom_bar(stat="identity",position = "dodge") + 
                labs(title = paste0(outcome),
                     subtitle = paste0(sexes ,collapse=", "))+
                scale_y_continuous(labels = function(x){format(x,big.mark=",", scientific = FALSE)},limits=plot.limits) + 
                theme(panel.background = element_blank(), legend.position = "bottom"
                      # panel.border = element_blank(), axis.line = element_line(color="gray")
                ) + 
                xlab("Age") 
        }
            
        

    }
    
}

generate.age.distribution.prev = function(results.array,
                                     intervention.1,
                                     year.1,
                                     intervention.2,
                                     year.2,
                                     intervention.3,
                                     year.3,
                                     percent=T,
                                     sexes=c("female","male"),
                                     plot.limits=c(0,200000)){
    results.array.1 = results.array[,,sexes,]
    results.array.2 = results.array[,,sexes,]
    results.array.3 = results.array[,,sexes,]
    
    # add back in sex dimension (if only one sex, will collapse over it)
    dim.names = dimnames(results.array)
    dim.names$sex = sexes
    dim(results.array.1) = dim(results.array.2) = dim(results.array.3) = sapply(dim.names,length)
    dimnames(results.array.1) = dimnames(results.array.2) = dimnames(results.array.3) = dim.names
    
    # get age counts
    age.counts.1 = apply(results.array.1[year.1,,,],c("age","sim"),sum)
    age.counts.2 = apply(results.array.2[year.2,,,],c("age","sim"),sum)
    age.counts.3 = apply(results.array.3[year.3,,,],c("age","sim"),sum)
    
    if(percent){
        # get totals 
        total.counts.1 = apply(age.counts.1,c("sim"),sum)
        total.counts.2 = apply(age.counts.2,c("sim"),sum)
        total.counts.3 = apply(age.counts.3,c("sim"),sum)
        
        age.proportions.1 = age.counts.1/rep(as.numeric(total.counts.1), each=length(dimnames(age.counts.1)[[1]]))
        age.proportions.2 = age.counts.2/rep(as.numeric(total.counts.2), each=length(dimnames(age.counts.1)[[1]]))
        age.proportions.3 = age.counts.3/rep(as.numeric(total.counts.3), each=length(dimnames(age.counts.1)[[1]]))
        
        # marginalizing over sim now
        age.summary.1 = apply(age.proportions.1,c("age"),quantile,probs=c(.025,.5,.975),na.rm=T)
        age.summary.2 = apply(age.proportions.2,c("age"),quantile,probs=c(.025,.5,.975),na.rm=T)
        age.summary.3 = apply(age.proportions.3,c("age"),quantile,probs=c(.025,.5,.975),na.rm=T)
    } else {
        # marginalizing over sim now
        age.summary.1 = apply(age.counts.1,c("age"),quantile,probs=c(.025,.5,.975),na.rm=T)
        age.summary.2 = apply(age.counts.2,c("age"),quantile,probs=c(.025,.5,.975),na.rm=T)
        age.summary.3 = apply(age.counts.3,c("age"),quantile,probs=c(.025,.5,.975),na.rm=T)
    }
    
    age.summary = c(age.summary.1,age.summary.2,age.summary.3)
    dim.names = list(stat = c("lower","median","upper"),
                     age=dimnames(age.counts.1)[[1]],
                     intervention=c(paste0(intervention.1,"/",year.1),
                                    paste0(intervention.2,"/",year.2),
                                    paste0(intervention.3,"/",year.3)))
    dim(age.summary) = sapply(dim.names,length)
    dimnames(age.summary) = dim.names
    
    age.summary = age.summary["median",,]
    df = melt(age.summary)
    
    if(percent){
        ggplot(data = df,aes(x=age,y=value,fill=intervention)) + 
            geom_bar(stat="identity",position = "dodge") + 
            labs(title = paste0("Prevalence"),
                 subtitle = paste0(sexes ,collapse=", "))+
            scale_y_continuous(labels = scales::percent,name = NULL,limits=plot.limits) + 
            theme(panel.background = element_blank(), legend.position = "bottom"
                  # panel.border = element_blank(), axis.line = element_line(color="gray")
            ) + 
            xlab("Age") 
    } else {
        ggplot(data = df,aes(x=age,y=value,fill=intervention)) + 
            geom_bar(stat="identity",position = "dodge") + 
            labs(title = paste0("Prevalence"),
                 subtitle = paste0(sexes ,collapse=", "))+
            scale_y_continuous(labels = function(x){format(x,big.mark=",")},limits=plot.limits) + 
            theme(panel.background = element_blank(), legend.position = "bottom"
                  # panel.border = element_blank(), axis.line = element_line(color="gray")
            ) + 
            xlab("Age") 
    }

    
}

generate.age.distribution.2.column = function(results.array,
                                              intervention.1,
                                              year.1,
                                              intervention.2,
                                              year.2,
                                              outcome,
                                              percent=T,
                                              display="figure"){
    
    results.array.1 = results.array[,,,,,intervention.1]
    results.array.2 = results.array[,,,,,intervention.2]
    
    # get age counts
    age.counts.1 = apply(results.array.1[year.1,,,outcome,],c("age","sim"),sum)
    age.counts.2 = apply(results.array.2[year.2,,,outcome,],c("age","sim"),sum)
    
    if(percent){
        # get totals 
        total.counts.1 = apply(age.counts.1,c("sim"),sum)
        total.counts.2 = apply(age.counts.2,c("sim"),sum)
        
        age.proportions.1 = age.counts.1/rep(as.numeric(total.counts.1), each=17)
        age.proportions.2 = age.counts.2/rep(as.numeric(total.counts.2), each=17)
        
        # marginalizing over sim now
        age.summary.1 = apply(age.proportions.1,c("age"),quantile,probs=c(.025,.5,.975),na.rm=T)
        age.summary.2 = apply(age.proportions.2,c("age"),quantile,probs=c(.025,.5,.975),na.rm=T)
        
    } else {
        # marginalizing over sim now
        age.summary.1 = apply(age.counts.1,c("age"),quantile,probs=c(.025,.5,.975),na.rm=T)
        age.summary.2 = apply(age.counts.2,c("age"),quantile,probs=c(.025,.5,.975),na.rm=T)
    }
    
    if(display=="table"){
        if(percent){
            tab.1 = c(paste0(round(100*age.summary.1[2,],1),"% [",
                             round(100*age.summary.1[1,],1),"-",
                             round(100*age.summary.1[3,],1),"]"),
                      paste0(round(100*age.summary.2[2,],1),"% [",
                             round(100*age.summary.2[1,],1),"-",
                             round(100*age.summary.2[3,],1),"]")) 
        } else{
            tab.1 = c(paste0(round(age.summary.1[2,],1)," [",
                             round(age.summary.1[1,],1),"-",
                             round(age.summary.1[3,],1),"]"),
                      paste0(round(age.summary.2[2,],1)," [",
                             round(age.summary.2[1,],1),"-",
                             round(age.summary.2[3,],1),"]"))
        }

        tab.dim.names.1 = list(age=dimnames(age.counts.1)[[1]],
                               intervention=c(paste0(intervention.1,"/",year.1),
                                              paste0(intervention.2,"/",year.2)))
        dim(tab.1) = sapply(tab.dim.names.1,length)
        dimnames(tab.1) = tab.dim.names.1
        
        return(tab.1)
    } else if(display=="numeric"){
        rv = list()
        rv$year.1 = age.summary.1
        rv$year.2 = age.summary.2
        rv$year.1.number.over.50 = apply(rv$year.1[,c(11:17)],1,sum)
        rv$year.2.number.over.50 = apply(rv$year.2[,c(11:17)],1,sum)
        rv$year.1.number.over.65 = apply(rv$year.1[,c(14:17)],1,sum)
        rv$year.2.number.over.65 = apply(rv$year.2[,c(14:17)],1,sum)
        
        return(rv)
    }else if(display=="figure"){
        age.summary = c(age.summary.1,age.summary.2)
        dim.names = list(stat = c("lower","median","upper"),
                         age=dimnames(age.counts.1)[[1]],
                         intervention=c(paste0(intervention.1,"/",year.1),
                                        paste0(intervention.2,"/",year.2)))
        dim(age.summary) = sapply(dim.names,length)
        dimnames(age.summary) = dim.names
        
        age.summary = age.summary["median",,]
        df = melt(age.summary)
        
        ggplot(data = df,aes(x=age,y=value,fill=intervention)) + 
            geom_bar(stat="identity",position = "dodge") + 
            ggtitle(paste0(outcome)) + 
            theme(panel.background = element_blank(), legend.position = "bottom"
                  # panel.border = element_blank(), axis.line = element_line(color="gray")
            ) + 
            xlab("Age")
    }
    
}


# calculates the median value based on the counts in each category 
# (e.g., what's the median age if there are 10 individuals age 0-4, 7 age 5-9, 3 age 10-14, etc.)
calculate.median = function(counts, # counts in each value category 
                            values){ # values of each category 
    
    cumulative.probs = cumsum(counts)/sum(counts)
    
    mask = cumulative.probs>=0.5
    mask.2 = cumulative.probs>0.5
    
    value.1 = values[mask][1]
    value.2 = values[mask.2][1]
    
    (value.1 + value.2)/2
    
}

# uses calculate.median on an extracted datatype 
calculate.median.age.for.sim = function(sim,
                                        data.type,
                                        year,
                                        sexes){
    if(length(year)!=1)
        stop("can only pass one year at a time to calculate.median.age.for.sim")
    
    counts.by.age.bracket = extract.data(sim = sim,
                                         data.type = data.type,
                                         years = year,
                                         sexes = sexes,
                                         keep.dimensions = c("year","age"))
    
    counts.by.age.bracket = colSums(counts.by.age.bracket) # sum over years
    
    counts.by.age = unlist(sapply(1:length(counts.by.age.bracket), function(age.bracket.index){
        
        num.ages.in.bracket = sim$parameters$AGE.SPANS[age.bracket.index]
        if(is.infinite(num.ages.in.bracket)) # upper age bracket 
            num.ages.in.bracket=1 # median is never going to be in the highest age bracket so this is fine 
        rv = rep(counts.by.age.bracket[age.bracket.index]/num.ages.in.bracket,num.ages.in.bracket)
        names(rv) = as.character(sim$parameters$AGE.LOWERS[age.bracket.index]+0:(num.ages.in.bracket-1))
        
        rv
        
    }))
    
    calculate.median(counts=counts.by.age,
                     values=as.numeric(names(counts.by.age)))
}

# calculates the percentage of people who are above a certain age threshold (e.g., 60% of PLHIV are over age 50)
calculate.percent.over.age.for.sim = function(sim,
                                              age.point,
                                              data.type,
                                              year,
                                              sexes){
    if(length(year)!=1)
        stop("can only pass one year at a time to calculate.percent.over.age.for.sim")
    
    counts.by.age.bracket = extract.data(sim = sim,
                                         data.type = data.type,
                                         years = year,
                                         sexes = sexes,
                                         keep.dimensions = c("year","age"))
    
    counts.by.age.bracket = colSums(counts.by.age.bracket) # sum over years
    
    counts.by.age = unlist(sapply(1:length(counts.by.age.bracket), function(age.bracket.index){
        
        num.ages.in.bracket = sim$parameters$AGE.SPANS[age.bracket.index]
        if(is.infinite(num.ages.in.bracket)) # upper age bracket 
            num.ages.in.bracket=1 # median is never going to be in the highest age bracket so this is fine 
        rv = rep(counts.by.age.bracket[age.bracket.index]/num.ages.in.bracket,num.ages.in.bracket)
        names(rv) = as.character(sim$parameters$AGE.LOWERS[age.bracket.index]+0:(num.ages.in.bracket-1))
        
        rv
        
    }))
    
    sum(counts.by.age[as.numeric(names(counts.by.age))>=age.point])/sum(counts.by.age.bracket)
    
}

# calculates the number of people who are above a certain age threshold (e.g., XXX of PLHIV are over age 50)
calculate.number.over.age.for.sim = function(sim,
                                              age.point,
                                              data.type,
                                              year,
                                              sexes){
    if(length(year)!=1)
        stop("can only pass one year at a time to calculate.number.over.age.for.sim")
    
    counts.by.age.bracket = extract.data(sim = sim,
                                         data.type = data.type,
                                         years = year,
                                         sexes = sexes,
                                         keep.dimensions = c("year","age"))
    
    counts.by.age.bracket = colSums(counts.by.age.bracket) # sum over years
    
    counts.by.age = unlist(sapply(1:length(counts.by.age.bracket), function(age.bracket.index){
        
        num.ages.in.bracket = sim$parameters$AGE.SPANS[age.bracket.index]
        if(is.infinite(num.ages.in.bracket)) # upper age bracket 
            num.ages.in.bracket=1 # median is never going to be in the highest age bracket so this is fine 
        rv = rep(counts.by.age.bracket[age.bracket.index]/num.ages.in.bracket,num.ages.in.bracket)
        names(rv) = as.character(sim$parameters$AGE.LOWERS[age.bracket.index]+0:(num.ages.in.bracket-1))
        
        rv
        
    }))
    
    sum(counts.by.age[as.numeric(names(counts.by.age))>=age.point])
    
}

# applies calculate.median.age.for.sim over a simset
calculate.median.age.for.simset = function(simset,
                                           data.type,
                                           year,
                                           sexes){
    if(length(year)!=1)
        stop("can only pass one year at a time to calculate.median.age.for.simset")
    
    rv = sapply(simset@simulations, function(sim){
        calculate.median.age.for.sim(sim, 
                                     data.type = data.type,
                                     year = year,
                                     sexes = sexes)
    })
    
    rv = quantile(rv,probs=c(.025,.5,.975), na.rm=T)
    
    rv
}

# applies calculate.percent.over.age.for.sim over a simset
calculate.percent.over.age.for.simset = function(simset,
                                                 age.point,
                                                 data.type,
                                                 year,
                                                 sexes){
    if(length(year)!=1)
        stop("can only pass one year at a time to calculate.percent.over.age.for.simset")
    
    rv = sapply(simset@simulations, function(sim){
        calculate.percent.over.age.for.sim(sim, 
                                           age.point = age.point,
                                           data.type = data.type,
                                           year = year,
                                           sexes = sexes)
    })
    
    rv = quantile(rv,probs=c(.025,.5,.975), na.rm=T)
    
    rv
    
}

# applies calculate.number.over.age.for.sim over a simset
calculate.number.over.age.for.simset = function(simset,
                                                 age.point,
                                                 data.type,
                                                 year,
                                                 sexes){
    if(length(year)!=1)
        stop("can only pass one year at a time to calculate.number.over.age.for.simset")
    
    rv = sapply(simset@simulations, function(sim){
        calculate.number.over.age.for.sim(sim, 
                                           age.point = age.point,
                                           data.type = data.type,
                                           year = year,
                                           sexes = sexes)
    })
    
    rv = quantile(rv,probs=c(.025,.5,.975), na.rm=T)
    
    rv
    
}

generate.median.age.table = function(simset.list,
                                     data.types,
                                     years,
                                     sexes = c("female","male")){
    
    dim.names = list(intervention = names(simset.list),
                     year = years,
                     data.type = data.types)
    
    rv = sapply(data.types, function(d){
        sapply(years, function(y){
            sapply(simset.list, function(simset){
                
                paste0(round(calculate.median.age.for.simset(simset=simset,
                                                             data.type = d,
                                                             year = y,
                                                             sexes = sexes)[2])," [",
                       round(calculate.median.age.for.simset(simset=simset,
                                                             data.type = d,
                                                             year = y,
                                                             sexes = sexes)[1]),"-",
                       round(calculate.median.age.for.simset(simset=simset,
                                                             data.type = d,
                                                             year = y,
                                                             sexes = sexes)[3]),"]")
            })
        })
    })
    
    dim(rv) = sapply(dim.names,length)
    dimnames(rv) = dim.names
    
    rv
}


generate.percent.over.age.table = function(simset.list,
                                           age.point,
                                           data.types,
                                           years,
                                           sexes = c("female","male")){
    
    dim.names = list(intervention = names(simset.list),
                     year = years,
                     data.type = data.types)
    
    rv = sapply(data.types, function(d){
        sapply(years, function(y){
            sapply(simset.list, function(simset){
                
                paste0(round(100*calculate.percent.over.age.for.simset(simset=simset,
                                                                       age.point=age.point,
                                                                       data.type = d,
                                                                       year = y,
                                                                       sexes = sexes)[2]),"% [",
                       round(100*calculate.percent.over.age.for.simset(simset=simset,
                                                                       age.point=age.point,
                                                                       data.type = d,
                                                                       year = y,
                                                                       sexes = sexes)[1]),"-",
                       round(100*calculate.percent.over.age.for.simset(simset=simset,
                                                                       age.point=age.point,
                                                                       data.type = d,
                                                                       year = y,
                                                                       sexes = sexes)[3]),"]")
            })
        })
    })
    
    dim(rv) = sapply(dim.names,length)
    dimnames(rv) = dim.names
    
    rv
}

generate.number.over.age.table = function(simset.list,
                                           age.point,
                                           data.types,
                                           years,
                                           sexes = c("female","male")){
    
    dim.names = list(intervention = names(simset.list),
                     year = years,
                     data.type = data.types)
    
    rv = sapply(data.types, function(d){
        sapply(years, function(y){
            sapply(simset.list, function(simset){
                
                paste0(comma(round(calculate.number.over.age.for.simset(simset=simset,
                                                                       age.point=age.point,
                                                                       data.type = d,
                                                                       year = y,
                                                                       sexes = sexes)[2]))," [",
                       comma(round(calculate.number.over.age.for.simset(simset=simset,
                                                                       age.point=age.point,
                                                                       data.type = d,
                                                                       year = y,
                                                                       sexes = sexes)[1])),"-",
                       comma(round(calculate.number.over.age.for.simset(simset=simset,
                                                                       age.point=age.point,
                                                                       data.type = d,
                                                                       year = y,
                                                                       sexes = sexes)[3])),"]")
            })
        })
    })
    
    dim(rv) = sapply(dim.names,length)
    dimnames(rv) = dim.names
    
    rv
}

calculate.outcome.reduction = function(results.array,
                                       target.year,
                                       data.type,
                                       intervention,
                                       sexes = c("female","male")){
    
    base = results.array[target.year,,sexes,data.type,,"no.int"]
    int = results.array[target.year,,sexes,data.type,,intervention]
    
    base = apply(base,c("sim"),sum, na.rm=T) # get totals for each sim (sum over age/sex)
    int = apply(int,c("sim"),sum, na.rm = T)
    
    reduction = (base-int)/base
    reduction = quantile(reduction,probs=c(.025,.5,.975),na.rm=T)
    
    reduction
}
