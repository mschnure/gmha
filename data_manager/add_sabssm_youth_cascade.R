## NEXTGEN: South Africa 15-24 cascade calibration targets from the national household surveys (SABSSM V 2017, SABSSM VI 2022)
## Data: data_manager/data/sabssm_youth_cascade/sabssm_15-24_cascade_south_africa.csv
##
## Stored with the SAME definitions as the UNAIDS 95-95-95 cascade data already in the data manager:
##   awareness   = aware of status / all PLHIV                      (survey 1st 90/95)
##   engagement  = on ART / aware                                    (survey 2nd 90/95)
##   suppression = (VLS <1000 among on ART) x (on ART / aware)       (survey 3rd 90/95 x 2nd; denominator = aware,
##                 lowers = lower x engagement lower, uppers = upper x engagement upper - same as UNAIDS in
##                 read.surveillance.data)
## Survey definitions: aware = tested HIV+ and reported knowing status OR ARVs detected in blood; on ART = ARVs
## detected in blood (2022: self-report where no ARV result); VLS = <1000 copies/mL.
##
## 2017 (Marinda et al. 2020, Table 1): published 95% CIs, total and by sex.
## 2022 (SABSSM VI summary report, p23): point estimates only, total only (no sex-specific youth cascade published).
##   95% CIs derived on the logit scale from n with a design effect of 3.15, taken from the published 2022 15-24 VLS
##   estimate (70.1%, 63.3-76.1, n=614). n's: aware = 614 (15-24 PLHIV with VL result); on ART = 614 x 0.731;
##   VLS = 614 x 0.731 x 0.874.
##
## Adds age "15-24" to the awareness/engagement/suppression (+ .lowers/.uppers) year.age.location and
## year.age.sex.location arrays, for South Africa only (NA elsewhere, so other locations' likelihoods are unchanged).

add.age.to.array = function(arr, new.age, new.years = NULL){
    dn = dimnames(arr)
    if(!is.null(new.years))
        dn$year = as.character(sort(as.numeric(union(dn$year, new.years))))
    if(!(new.age %in% dn$age))
        dn$age = c(dn$age, new.age)
    rv = array(NA_real_, dim = sapply(dn, length), dimnames = dn)
    old.dn = dimnames(arr)
    # copy old values in
    idx = lapply(names(dn), function(d) match(old.dn[[d]], dn[[d]]))
    rv = do.call(`[<-`, c(list(rv), idx, list(value = arr)))
    rv
}

add.sabssm.youth.cascade = function(dm,
                                    file = "data_manager/data/sabssm_youth_cascade/sabssm_15-24_cascade_south_africa.csv",
                                    location = "South Africa",
                                    age = "15-24"){
    df = read.csv(file, stringsAsFactors = F)
    df$year = as.character(df$year)
    years = unique(df$year)

    get.val = function(yr, sx, ind, col){
        x = df[df$year==yr & df$sex==sx & df$indicator==ind, col]
        if(length(x)==0) NA_real_ else x/100
    }

    data.types = c("awareness","engagement","suppression")
    suffixes = c("" , ".lowers", ".uppers")
    cols = c("estimate","lower","upper")

    for(d in data.types){
        for(s in seq_along(suffixes)){
            name = paste0(d, suffixes[s])
            col = cols[s]

            dm[[name]]$year.age.location = add.age.to.array(dm[[name]]$year.age.location, age, years)
            dm[[name]]$year.age.sex.location = add.age.to.array(dm[[name]]$year.age.sex.location, age, years)

            for(yr in years){
                for(sx in c("all","male","female")){
                    val = switch(d,
                                 awareness = get.val(yr, sx, "aware", col),
                                 engagement = get.val(yr, sx, "art_among_aware", col),
                                 suppression = get.val(yr, sx, "vls_among_art", col) * get.val(yr, sx, "art_among_aware", col))
                    if(sx=="all")
                        dm[[name]]$year.age.location[yr, age, location] = val
                    else
                        dm[[name]]$year.age.sex.location[yr, age, sx, location] = val
                }
            }

            if(!(age %in% dm[[name]]$AGES)){
                dm[[name]]$AGES = c(dm[[name]]$AGES, age)
                dm[[name]]$AGE.LOWERS = c(dm[[name]]$AGE.LOWERS, 15)
                dm[[name]]$AGE.UPPERS = c(dm[[name]]$AGE.UPPERS, 25)
            }
        }
    }

    attr(dm, "sabssm.youth.cascade") = TRUE
    dm
}
