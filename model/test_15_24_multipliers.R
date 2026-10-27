## NEXTGEN: quick check that the youth cascade multipliers move the matching age group's cascade and nothing else
## (10/26/26: also the disengagement and unsuppression multipliers for 0-14 and 15-24)
## (default parameters, South Africa; each multiplier set to 0.5 in turn). Takes ~1 minute.
source("model/run_systematic.R")
LOCATION = "South Africa"
params = create.model.parameters(location = LOCATION)

cascade.by.age = function(sim, ages){
    yrs = c(2017, 2022)
    get = function(type) as.numeric(extract.data(sim, type, years = yrs, ages = ages, keep.dimensions = "year"))
    prev = get("prevalence"); aware = get("awareness"); eng = get("engagement"); supp = get("suppression")
    round(c(aware.2017 = aware[1]/prev[1], aware.2022 = aware[2]/prev[2],
            eng.2017 = eng[1]/aware[1], eng.2022 = eng[2]/aware[2],
            supp.2017 = supp[1]/aware[1], supp.2022 = supp[2]/aware[2]), 3)
}

runs = list(baseline = c(),
            awareness.0.5 = c(age.15.24.awareness.multiplier = 0.5),
            engagement.0.5 = c(age.15.24.engagement.multiplier = 0.5),
            suppression.0.5 = c(age.15.24.suppression.multiplier = 0.5),
            engagement.slope.minus.0.2 = c(log.OR.15.24.engagement.slope = -0.2),
            suppression.slope.minus.0.2 = c(log.OR.15.24.suppression.slope = -0.2),
            # 10/27/26: youth dropout multipliers (both dropout rates), x3 so the effect is easy to see
            disengagement.15.24.x3 = c(age.15.24.disengagement.multiplier = 3),
            disengagement.0.14.x3 = c(age.0.14.disengagement.multiplier = 3))
out = lapply(runs, function(v){
    sim = run.model.for.parameters(variable.parameters = v, parameters = params, location = LOCATION)
    rbind("0-14" = cascade.by.age(sim, c("0-4","5-9","10-14")),
          "15-24" = cascade.by.age(sim, c("15-19","20-24")),
          "25-49" = cascade.by.age(sim, c("25-29","30-34","35-39","40-44","45-49")))
})
for(n in names(out)){ cat("\n==", n, "==\n"); print(out[[n]]) }
