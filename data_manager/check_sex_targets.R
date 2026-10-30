################################################################################################
# 10/30/26: check the UNAIDS by-sex targets (AIDSinfo by-sex files) in the newest revised data manager.
# For each data type and location: total vs male + female (ratio should be ~1; the by-sex files can come from a
# newer UNAIDS estimates round than the totals, so differences point to mixed rounds), for a few years, and how
# many by-sex observations there are in the calibration years (1995-2023).
# Run after data_manager/save_data_manager.R and data_manager/revise_overlapping_age_groups.R.
################################################################################################
dm.files = list.files("cached", pattern = "^data\\.manager_global_revised_ages_(lai_)?[0-9-]+\\.Rdata$")
dm.file = file.path("cached", dm.files[order(sub(".*_([0-9-]+)\\.Rdata$", "\\1", dm.files))][length(dm.files)])
print(paste0("Checking ", dm.file))
load(dm.file)

LOCATIONS = c("South Africa","Kenya","Nigeria")
YEARS = c("2000","2010","2020","2023")
CALIBRATION.YEARS = as.character(1995:2023)
options(width = 200)
for(data.type in c("incidence","prevalence","hiv.mortality")){
    by.sex = DATA.MANAGER[[data.type]]$year.age.sex.location
    total = DATA.MANAGER[[data.type]]$year.age.location
    sex.all.ages = DATA.MANAGER[[data.type]]$year.sex.location
    if(is.null(by.sex)) { print(paste0(data.type, ": NO by-sex data")); next }
    for(location in LOCATIONS){
        ages = intersect(DATA.MANAGER[[data.type]]$AGES, dimnames(by.sex)$age)
        rows = lapply(ages, function(age) sapply(YEARS, function(y){
            m = by.sex[y, age, "male", location]; f = by.sex[y, age, "female", location]; t = total[y, age, location]
            if(is.na(m) && is.na(f)) return("no by-sex data")
            sprintf("M %s  F %s  (M+F)/total %.2f", format(round(m), big.mark = ","), format(round(f), big.mark = ","), (m+f)/t)
        }))
        tab = do.call(rbind, rows); dimnames(tab) = list(age = ages, year = YEARS)
        all.ages = sapply(YEARS, function(y) sprintf("M %s  F %s", format(round(sex.all.ages[y, "male", location]), big.mark = ","),
                                                      format(round(sex.all.ages[y, "female", location]), big.mark = ",")))
        tab = rbind(tab, "all ages" = all.ages)
        n.obs = sum(!is.na(by.sex[intersect(CALIBRATION.YEARS, dimnames(by.sex)$year), ages, , location])) +
            sum(!is.na(sex.all.ages[intersect(CALIBRATION.YEARS, dimnames(sex.all.ages)$year), , location]))
        print(paste0("==== ", data.type, ", ", location, ": ", n.obs, " by-sex observations in 1995-2023"))
        print(noquote(tab))
    }
}
