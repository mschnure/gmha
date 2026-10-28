################################################################################################
# NEXTGEN (10/28/26): print the South Africa cascade values behind manuscript Table 1 (LAI_gmha), straight from
# the model code, so the table can be checked against it:
#   testing (DHS model), engagement and suppression (IeDEA models), disengagement and rebound priors,
#   and the calibrated adjustments (prior median and +/- 2 SD range) - in the same terms as the table.
# Run on the nextgen branch:  source("calibration/print_table1_prior_values.R")
################################################################################################
source("source_code.R")
expit = function(x) 1/(1+exp(-x))
r3 = function(x) round(x, 3)
LOC = "South Africa"

cat("\n==== TESTING (probability of a test in the past year; DHS model) ====\n")
tm = get.testing.model(location = LOC)
p.test = function(age, sex, year) tm$max.proportion*expit(tm$intercepts[age, sex, 1] + tm$slopes[age, sex, 1]*(year - tm$anchor.year))
cat("anchor year:", tm$anchor.year, " max proportion:", tm$max.proportion, "\n")
for(sex in c("female","male")){
    cat(sex, ": age 35-39, 2015 =", r3(p.test("35-39", sex, 2015)),
        " | age 15-19 (17 and younger group), 2015 =", r3(p.test("15-19", sex, 2015)), "\n")
    cat("   yearly odds ratio: age 35-39 =", r3(exp(tm$slopes["35-39", sex, 1])),
        " | age 15-19 (17 and younger) =", r3(exp(tm$slopes["15-19", sex, 1])), "\n")
    # age effect for over 17: refit as in testing_projection_sa.R (age coefficient, at the anchor year)
    df = tm$master.df
    fit = suppressWarnings(glm(pmin(prop/tm$max.proportion,1) ~ year*age, family = binomial, data = df[df$sex==sex & df$age!=17,]))
    cat("   odds ratio per one-year increase in age (over 17), at", tm$anchor.year, "=", r3(exp(fit$coefficients["age"])),
        "; at 2015 =", r3(exp(fit$coefficients["age"] + fit$coefficients["year:age"]*(2015 - tm$anchor.year))), "\n")
}

print.iedea = function(m, name){
    cat("\n==== ", name, " (IeDEA model) ====\n", sep = "")
    cat("anchor year:", m$anchor.year, " max proportion:", m$max.proportion, "\n")
    p30 = function(year, female = 1) m$max.proportion*expit(m$intercept + m$slope*(year - m$anchor.year) + female*m$sex.female)
    cat("female, age 30-39, 2015 =", r3(p30(2015)), " | male, age 30-39, 2015 =", r3(p30(2015, 0)), "\n")
    cat("yearly odds ratio (all ages) =", r3(exp(m$slope)), "\n")
    cat("odds ratio vs 30-39: 10-19 =", r3(exp(m$age.10.19)), "; 20-29 =", r3(exp(m$age.20.29)),
        "; 40-49 =", r3(exp(m$age.40.49)), "; 50+ =", r3(exp(m$age.50.plus)), "\n")
    cat("odds ratio female vs male =", r3(exp(m$sex.female)), "\n")
}
print.iedea(get.engagement.model(location = LOC), "ENGAGEMENT (annual probability of starting ART)")
print.iedea(get.suppression.rates(location = LOC), "SUPPRESSION (annual probability of becoming suppressed)")

cat("\n==== PRIORS (South Africa): median and +/- 2 SD range ====\n")
prior = get.location.prior(LOC)
med = get.medians(prior); sds = get.sds(prior)
tf = unlist(sapply(prior@subdistributions, function(dist){
    if(.hasSlot(dist,"transformations")) sapply(dist@transformations, function(t) t@name)
    else if(is.null(dist@transformation)) "identity" else dist@transformation@name }))
names(tf) = prior@var.names
show = prior@var.names[prior@var.names %in% c("unsuppressed.disengagement.rates","suppressed.disengagement.rates","unsuppression.rates",
                     "log.OR.testing.intercept","log.OR.testing.slope","log.OR.engagement.slope","log.OR.suppression.slope",
                     "male.awareness.multiplier","male.engagement.multiplier","male.suppression.multiplier") |
                     grepl("^age\\.(0\\.14|15\\.24)\\.|^log\\.OR\\.(0\\.14|15\\.24)\\.", prior@var.names)]
cat("(sd shown on the log scale for lognormal priors; check: male.awareness.multiplier should show sd = log(8)/2 = 1.04)\n")
for(nm in show){
    m = med[nm]; s = sds[nm]
    if(tf[nm] == "log"){ # get.sds gives the lognormal's natural-scale sd: recover sdlog from median and sd
        x = (1 + sqrt(1 + 4*(s/m)^2))/2; s = sqrt(log(x))
        q = exp(log(m) + c(0, -2, 2)*s); lab = "value" }
    else { q = exp(m + c(0, -2, 2)*s); lab = "odds ratio (exp of log OR)" }
    line = sprintf("%-50s [%s, sd %.3f] %s: %s", nm, tf[nm], s, lab, paste(r3(q), collapse = " / "))
    if(grepl("rates$", nm)) line = paste0(line, "  -> annual probability: ", paste(r3(1 - exp(-q)), collapse = " / "))
    cat(line, "\n")
}
cat("\n(median / lower / upper)\n")
