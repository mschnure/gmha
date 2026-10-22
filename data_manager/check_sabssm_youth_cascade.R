## NEXTGEN: check the SA 15-24 cascade targets made it into the data manager, and show what the likelihood will use
source("source_code.R")
stopifnot(isTRUE(attr(DATA.MANAGER, "sabssm.youth.cascade")))

for(d in c("awareness","engagement","suppression")){
    cat("\n====", d, "(15-24, South Africa) ====\n")
    tab = cbind(estimate = DATA.MANAGER[[d]]$year.age.location[c("2017","2022"), "15-24", "South Africa"],
                lower = DATA.MANAGER[[paste0(d,".lowers")]]$year.age.location[c("2017","2022"), "15-24", "South Africa"],
                upper = DATA.MANAGER[[paste0(d,".uppers")]]$year.age.location[c("2017","2022"), "15-24", "South Africa"])
    print(round(tab, 3))
    cat("by sex, 2017:\n")
    print(round(DATA.MANAGER[[d]]$year.age.sex.location["2017", "15-24", , "South Africa"], 3))
    # 15+ UNAIDS data unchanged?
    cat("15+ (UNAIDS), 2017/2022:", round(DATA.MANAGER[[d]]$year.age.location[c("2017","2022"), "15+", "South Africa"], 3), "\n")
}

# observations the likelihood will see for South Africa (age and age/sex dimensions)
params = create.model.parameters(location = "South Africa")
for(d in c("awareness","engagement","suppression")){
    el = get.likelihood.elements.by.data.type(data.type = d, years = 1980:2030, location = "South Africa",
                                              data.manager = DATA.MANAGER, parameters = params,
                                              obs.correlation = 0.5, correlation.structure = "compound.symmetry",
                                              denominator.data.type = NULL, divide.obs.by.denominator = F)
    keep = grepl("15-24", el$obs.dimensions)
    cat("\n", d, ": ", sum(keep), " new 15-24 observations\n", sep = "")
    print(data.frame(year = el$obs.year[keep], strata = el$obs.dimensions[keep],
                     obs = round(el$obs[keep],3), sd = round(el$obs.sds[keep],3)))
}
