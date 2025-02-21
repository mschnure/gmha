
# assumes from is on the rows and to is on the columns
matrix.to.scatter <- function(mat,
                              age.cutoffs)
{
    rv = list(age.from=numeric(), age.to=numeric())
    for (i.from in 1:dim(mat)[1])
    {
        for (j.to in 1:dim(mat)[2])
        {
            n = mat[i.from,j.to]
            rv$age.from = c(rv$age.from, runif(n, age.cutoffs[i.from], age.cutoffs[i.from+1]))
            #            rv$age.from = c(rv$age.from, rep((age.cutoffs[i.from]+age.cutoffs[i.from+1])/2, n))
            rv$age.to = c(rv$age.to, runif(n, age.cutoffs[j.to], age.cutoffs[j.to+1]))
            #            rv$age.to = c(rv$age.from, rep((age.cutoffs[j.to]+age.cutoffs[j.to+1])/2, n))
        }
    }
    
    rv
}

fit.age.model <- function(age.of.reference, age.of.partner)
{
    diffs = age.of.partner - age.of.reference
    fit.for.mean = lm(diffs~age.of.reference)
    
    n.quantiles = 8
    cutoffs = quantile(age.of.reference, probs = seq(from=0,to=1,length=n.quantiles+1))
    midpoints = cutoffs[-1][-n.quantiles]
    cutoffs[n.quantiles] = Inf
    sds.for.fit = sapply(1:(n.quantiles-1), function(i){
        sd(diffs[age.of.reference>=cutoffs[i] & age.of.reference<cutoffs[i+2]])
    })
    fit.for.sd = lm(sds.for.fit~midpoints)
    
    rv=c(mean.intercept=as.numeric(fit.for.mean$coefficients[1]),
         mean.slope=as.numeric(fit.for.mean$coefficients[2]),
         sd.intercept=as.numeric(fit.for.sd$coefficients[1]),
         sd.slope=as.numeric(fit.for.sd$coefficients[2]))
    
    rv
}
